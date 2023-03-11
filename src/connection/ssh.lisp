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
;;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

(in-package :consfigurator.connection.ssh)
(named-readtables:in-readtable :consfigurator)

(defmethod establish-connection ((type (eql :ssh)) remaining
                                 &key
                                   (hop (get-hostname))
                                   user)
  (declare (ignore remaining))
  (informat 1 "~&Establishing SSH connection to ~A" hop)
  (aprog1 (make-instance 'ssh-connection :hostname hop :user user)
    (mrun "ssh" (ssh-host it) ":")))

(defclass ssh-connection (shell-wrap-connection)
  ((hostname
    :initarg :hostname
    :documentation "Hostname to SSH to.")
   ;; This is deliberately distinct from the :REMOTE-USER connattr.
   (user
    :initarg :user
    :documentation "User to log in as."))
  (:documentation "Deploy properties using non-interactive SSH."))

(defun ssh-host (connection)
  (if-let ((user (slot-value connection 'user)))
    (format nil "~A@~A" user (slot-value connection 'hostname))
    (slot-value connection 'hostname)))

(defmethod connection-shell-wrap ((connection ssh-connection) cmd)
  ;; wrap in 'sh -c' in case the login shell is not POSIX
  (format nil "ssh ~A ~A"
          (ssh-host connection)
          (sh-escape (format nil "sh -c ~A" (sh-escape cmd)))))
