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
#+sbcl (eval-when (:compile-toplevel :load-toplevel :execute)
         (require "sb-posix"))

(defun chroot (path)
  #+sbcl      (sb-posix:chroot path)
  #-(or sbcl) (foreign-funcall "chroot" :string path :int))

(defmethod establish-connection ((type (eql :chroot.fork)) remaining &key into)
  (unless (and (lisp-connection-p) (zerop (foreign-funcall "geteuid" :int)))
    (error "~&Forking into a chroot requires a Lisp image running as root"))
  (informat 1 "~&Forking into chroot at ~A" into)
  (with-fork-connection (remaining)
      (unless (zerop (chroot into))
	(error "chroot(2) failed; are you root?"))
    ;; chdir, else our current working directory is a pointer to something
    ;; outside the chroot
    (uiop:chdir "/")))
