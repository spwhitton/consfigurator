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

(in-package :consfigurator.connection.chroot.shell)
(named-readtables:in-readtable :consfigurator)

(defmethod establish-connection ((type (eql :chroot.shell)) remaining &key into)
  (declare (ignore remaining))
  (format t "Shelling into chroot at ~A~%" into)
  (make-instance 'shell-chroot-connection :root into))

(defclass shell-chroot-connection (shell-wrap-connection)
  ((root
    :initarg :root)))

(defmethod connection-shell-wrap ((connection shell-chroot-connection) cmd)
  (format nil "chroot ~A sh -c ~A"
	  (escape-sh-token (slot-value connection 'root))
	  (escape-sh-token cmd)))

(defmethod connection-upload ((connection shell-chroot-connection) from to)
  (mrun "cp" from (merge-pathnames to (ensure-directory-pathname
				       (slot-value connection 'root)))))
