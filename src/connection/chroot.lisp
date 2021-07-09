;;; Consfigurator -- Lisp declarative configuration management system

;;; Copyright (C) 2021  Sean Whitton <spwhitton@spwhitton.name>

;;; This file is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3, or (at your option)
;;; any later version.

;;; This file is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.

;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(in-package :consfigurator.connection.chroot)
(named-readtables:in-readtable :consfigurator)

;; currently we only check whether we're root, but, for example, on Linux, we
;; might have a CAP_* which lets us chroot as non-root
(defun can-chroot ()
  (zerop (foreign-funcall "geteuid" :int)))

(defmethod establish-connection ((type (eql :chroot)) remaining &key into)
  (establish-connection (if (and (lisp-connection-p)
                                 (can-chroot)
                                 (can-probably-fork))
                            :chroot.fork
                            :chroot.shell)
                        remaining
                        :into into))


;;;; Chroot connections superclass

(defclass chroot-connection ()
  ((into :type :string :initarg :into)
   (chroot-mounts :type list :initform nil :accessor chroot-mounts)))

(defgeneric chroot-mount (connection &rest mount-args)
  (:documentation
   "Temporarily mount something into the chroot.  The last element of MOUNT-ARGS
should be the mount point, without the chroot's root prefixed.")
  (:method ((connection chroot-connection) &rest mount-args)
    (let ((dest (chroot-pathname (lastcar mount-args)
                                 (slot-value connection 'into))))
      ;; We only mount when the target is not already a mount point, so we
      ;; don't shadow anything that the user has already set up.
      (when (plusp (mrun :for-exit "mountpoint" "-q" dest))
        (setq mount-args (copy-list mount-args))
        (setf (lastcar mount-args) dest)
        (apply #'mrun "mount" mount-args)
        (push dest (chroot-mounts connection))))))

(defmethod connection-teardown :before ((connection chroot-connection))
  (dolist (mount (chroot-mounts connection))
    ;; There shouldn't be any processes left running in the chroot after we've
    ;; finished deploying it, but it's quite easy to end up with things like
    ;; gpg-agent holding on to /dev/null, for example, so for simplicity, do a
    ;; lazy unmount.
    (mrun "umount" "-l" mount)))

(defmethod initialize-instance :after ((connection chroot-connection) &key)
  (when (string= "Linux" (stripln (run "uname")))
    (with-slots (into) connection
      ;; Ensure the chroot itself is a mountpoint so that findmnt(8) works
      ;; correctly within the chroot.
      (unless (zerop (mrun :for-exit "mountpoint" "-q" into))
        (chroot-mount connection "--bind" into "/"))
      ;; Now set up the usual bind mounts.  Help here from arch-chroot(8).
      (mount:assert-devtmpfs-udev-/dev)
      (dolist (mount mount:*standard-linux-vfs*)
        (apply #'chroot-mount connection mount))
      (when (remote-exists-p "/sys/firmware/efi/efivars")
        (apply #'chroot-mount connection mount:*linux-efivars-vfs*)))))

(defmethod propagate-connattr
    ((type (eql :opened-volumes)) connattr (connection chroot-connection))
  (with-slots (into) connection
    (loop for volume in connattr
          when (and (subtypep (type-of volume) 'disk:filesystem)
                    (slot-boundp volume 'disk:mount-point)
                    (subpathp (disk:mount-point volume) into))
            collect (let ((copy (disk:copy-volume-and-contents volume)))
                      (setf (disk:mount-point copy)
                            (in-chroot-pathname (disk:mount-point copy) into))
                      copy)
          else collect volume)))

(defmethod propagate-connattr
    ((type (eql :remote-uid)) connattr (connection chroot-connection))
  connattr)

(defmethod propagate-connattr
    ((type (eql :remote-gid)) connattr (connection chroot-connection))
  connattr)


;;;; :CHROOT.FORK

(defun chroot (path)
  #+sbcl      (sb-posix:chroot path)
  #-(or sbcl) (foreign-funcall "chroot" :string path :int))

(defclass chroot.fork-connection
    (rehome-connection chroot-connection fork-connection) ())

(defmethod establish-connection ((type (eql :chroot.fork)) remaining &key into)
  (unless (and (lisp-connection-p) (zerop (foreign-funcall "geteuid" :int)))
    (error "~&Forking into a chroot requires a Lisp image running as root"))
  (informat 1 "~&Forking into chroot at ~A" into)
  (let* ((into* (ensure-directory-pathname into))
         (connection (make-instance 'shell-chroot-connection :into into*)))
    ;; This has the side effect of populating the CONSFIGURATOR::ID and
    ;; :REMOTE-HOME connattrs correctly, so that they don't get bogus values
    ;; when this connection object is used in UPLOAD-ALL-PREREQUISITE-DATA.
    (multiple-value-bind (datadir-inside exit)
        (connection-run
         connection
         (format nil "echo ${XDG_CACHE_HOME:-~A/.cache}/consfigurator/data/"
                 (connection-connattr connection :remote-home))
         nil)
      (unless (zerop exit)
        (error "Failed to determine datadir inside chroot."))
      (setq connection (change-class connection 'chroot.fork-connection))
      (setf (slot-value connection 'datadir)
            (ensure-pathname
             (stripln (subseq datadir-inside 1))
             :defaults into* :ensure-absolute t :ensure-directory t))
      (unwind-protect-in-parent (continue-connection connection remaining)
        (connection-teardown connection)))))

(defmethod post-fork ((connection chroot.fork-connection))
  (unless (zerop (chroot (slot-value connection 'into)))
    (error "chroot(2) failed!"))
  (let ((home (connection-connattr connection :remote-home)))
    (setf (getenv "HOME") (unix-namestring home))
    ;; chdir, else our current working directory is a pointer to something
    ;; outside the chroot
    (uiop:chdir home)))


;;;; :CHROOT.SHELL

(defmethod establish-connection ((type (eql :chroot.shell)) remaining &key into)
  (declare (ignore remaining))
  (informat 1 "~&Shelling into chroot at ~A" into)
  (make-instance 'shell-chroot-connection :into into))

(defclass shell-chroot-connection (chroot-connection shell-wrap-connection) ())

(defmethod connection-shell-wrap ((connection shell-chroot-connection) cmd)
  (format nil "chroot ~A sh -c ~A"
          (escape-sh-token (unix-namestring (slot-value connection 'into)))
          (escape-sh-token cmd)))
