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

(in-package :consfigurator.connection.ssh)
(named-readtables:in-readtable :consfigurator)

(defmethod establish-connection ((type (eql :ssh)) remaining
                                 &key
                                   (hop (get-hostname))
                                   user)
  (declare (ignore remaining))
  (informat 1 "~&Establishing SSH connection to ~A" hop)
  (mrun "ssh" "-fN" hop)
  (make-instance 'ssh-connection :hostname hop :user user))

(defclass ssh-connection (shell-wrap-connection)
  ((hostname
    :initarg :hostname
    :documentation "Hostname to SSH to.")
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
          (escape-sh-token (format nil "sh -c ~A" (escape-sh-token cmd)))))

;; rsync it straight to to its destination so rsync can do incremental updates
(defmethod connection-upload ((c ssh-connection) from to)
  (mrun "rsync" "-Pavc" from (format nil "~A:~A" (ssh-host c) to)))
