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
  ((into :type :string :initarg :into)))


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
         (datadir-inside
           (stripln
            (mrun
             "chroot" into
             "echo" "${XDG_CACHE_HOME:-$HOME/.cache}/consfigurator/data/")))
         (datadir (ensure-pathname
                   (subseq datadir-inside 1)
                   :defaults into* :ensure-absolute t :ensure-directory t)))
    (continue-connection
     (make-instance 'chroot.fork-connection :into into :datadir datadir)
     remaining)))

(defmethod post-fork ((connection chroot.fork-connection))
  (unless (zerop (chroot (slot-value connection 'into)))
    (error "chroot(2) failed!"))
  ;; chdir, else our current working directory is a pointer to something
  ;; outside the chroot
  (uiop:chdir "/"))


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
