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
  (zerop (nix:geteuid)))

(defmethod establish-connection ((type (eql :chroot)) remaining &key into)
  (establish-connection (if (and (lisp-connection-p) (can-chroot))
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
      (unless (remote-mount-point-p dest)
        (setq mount-args (copy-list mount-args))
        (setf (lastcar mount-args) dest)
        (apply #'mrun "mount" mount-args)
        (push dest (chroot-mounts connection))))))

(defgeneric linux-chroot-mounts (connection)
  (:method ((connection chroot-connection))
    (with-slots (into) connection
      ;; Ensure the chroot itself is a mountpoint so that findmnt(8) works
      ;; correctly within the chroot.
      (unless (remote-mount-point-p into)
        (chroot-mount connection "--bind" into "/"))
      ;; Now set up the usual bind mounts.  Help here from arch-chroot(8).
      (mount:assert-devtmpfs-udev-/dev)
      (dolist (mount mount:*linux-basic-vfs*)
        (apply #'chroot-mount connection mount))
      (chroot-mount connection "--bind" "/run" "/run")
      (when (remote-exists-p "/sys/firmware/efi/efivars")
        (apply #'chroot-mount connection mount:*linux-efivars-vfs*)))))

(defmethod propagate-connattr
    ((type (eql :opened-volumes)) connattr (connection chroot-connection))
  (with-slots (into) connection
    (loop for volume in connattr
          when (and (subtypep (type-of volume) 'disk:filesystem)
                    (slot-boundp volume 'disk:mount-point)
                    (subpathp (disk:mount-point volume) into))
            collect (aprog1 (disk:copy-volume-and-contents volume)
                      (setf (disk:mount-point it)
                            (in-chroot-pathname (disk:mount-point it) into)))
          else collect volume)))

(defmethod propagate-connattr
    ((type (eql :remote-uid)) connattr (connection chroot-connection))
  connattr)

(defmethod propagate-connattr
    ((type (eql :remote-gid)) connattr (connection chroot-connection))
  connattr)

(defmethod propagate-connattr
    ((type (eql :no-services)) connattr (connection chroot-connection))
  connattr)


;;;; :CHROOT.FORK

(defclass chroot.fork-connection
    (rehome-connection chroot-connection fork-connection) ())

(defmethod establish-connection ((type (eql :chroot.fork)) remaining &key into)
  (unless (and (lisp-connection-p) (zerop (nix:geteuid)))
    (error "~&Forking into a chroot requires a Lisp image running as root"))
  (informat 1 "~&Forking into chroot at ~A" into)
  (let* ((into (ensure-pathname into
				:defaults (uiop:getcwd)
				:ensure-absolute t :ensure-directory t))
         (connection (make-instance 'shell-chroot-connection :into into)))
    ;; Populate the CONSFIGURATOR::ID and :REMOTE-HOME connattrs correctly to
    ;; ensure they don't get bogus values when this connection object is used
    ;; in UPLOAD-ALL-PREREQUISITE-DATA.
    (connection-connattr connection :remote-home)
    ;; Obtain & cache XDG_CACHE_HOME inside the chroot, and compute DATADIR.
    (let ((xdg-cache-home (connection-connattr connection :XDG_CACHE_HOME)))
      (setf connection (change-class connection 'chroot.fork-connection)
            (slot-value connection 'datadir)
            (merge-pathnames
             "consfigurator/data/" (chroot-pathname xdg-cache-home into))))
    (continue-connection connection remaining)))

(defmethod post-fork ((connection chroot.fork-connection))
  (with-slots (into) connection
    #+linux
    (progn (unshare +CLONE_NEWNS+)
           (mrun "mount" "--make-rslave"
                 (stripln (run "findmnt" "-nro" "TARGET" "-T" into)))
           (linux-chroot-mounts connection))
    (chroot (unix-namestring into))
    (let ((home (connection-connattr connection :remote-home)))
      (setf (getenv "HOME") (unix-namestring home))
      ;; chdir, else our current working directory is a pointer to something
      ;; outside the chroot
      (uiop:chdir home))))


;;;; :CHROOT.SHELL

(defmethod establish-connection ((type (eql :chroot.shell)) remaining &key into)
  (declare (ignore remaining))
  (informat 1 "~&Shelling into chroot at ~A" into)
  (aprog1 (make-instance 'shell-chroot-connection :into into)
    (when (string= "Linux" (stripln (run "uname")))
      (linux-chroot-mounts it))))

(defclass shell-chroot-connection (chroot-connection shell-wrap-connection) ())

(defmethod connection-shell-wrap ((connection shell-chroot-connection) cmd)
  (format nil "chroot ~A sh -c ~A"
          (sh-escape (slot-value connection 'into)) (sh-escape cmd)))

(defmethod connection-tear-down :before ((connection shell-chroot-connection))
  (dolist (mount (chroot-mounts connection))
    ;; There shouldn't be any processes left running in the chroot after we've
    ;; finished deploying it, but it's quite easy to end up with things like
    ;; gpg-agent holding on to /dev/null, for example, so for simplicity, do a
    ;; lazy unmount.
    (mrun "umount" "-l" mount)))
