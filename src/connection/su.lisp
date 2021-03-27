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

(in-package :consfigurator.connection.su)
(named-readtables:in-readtable :consfigurator)

(defmethod establish-connection ((type (eql :su)) remaining &key to)
  (declare (ignore remaining))
  ;; We don't support using su with a password.  Use :SUDO for that.
  (assert-euid-root)
  (informat 1 "~&Switching to user ~A" to)
  (make-instance 'su-connection :user to))

(defclass su-connection (shell-wrap-connection)
  ((user :initarg :user)))

(defmethod connection-shell-wrap ((connection su-connection) cmd)
  (format nil "su ~A -c ~A"
          (escape-sh-token (slot-value connection 'user))
          (escape-sh-token cmd)))
