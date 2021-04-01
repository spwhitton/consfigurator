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

(in-package :consfigurator.connection.chroot.fork)
(named-readtables:in-readtable :consfigurator)

(defun chroot (path)
  #+sbcl      (sb-posix:chroot path)
  #-(or sbcl) (foreign-funcall "chroot" :string path :int))

(defclass chroot.fork-connection (rehome-connection fork-connection)
  ((into :type :string :initarg :into)))

(defmethod establish-connection ((type (eql :chroot.fork)) remaining &key into)
  (unless (and (lisp-connection-p) (zerop (foreign-funcall "geteuid" :int)))
    (error "~&Forking into a chroot requires a Lisp image running as root"))
  (informat 1 "~&Forking into chroot at ~A" into)
  (let* ((into* (ensure-pathname into))
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
