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

(named-readtables:in-readtable :interpol-syntax)

(defmethod establish-connection ((type (eql :ssh)) remaining
				 &key
				   (hop (get-hostname))
				   user)
  (declare (ignore remaining))
  (run "ssh" "-fN" hop)
  (make-instance 'ssh-connection :hostname hop :user user))

(defclass ssh-connection (posix-connection)
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

(defun sshcmd (connection &rest args)
  ;; wrap in 'sh -c' in case the login shell is not POSIX
  (format nil "ssh ~A ~A"
	  (ssh-host connection)
	  (escape-sh-token
	   (format nil "sh -c ~A"
		   (escape-sh-token
		    (if (cdr args) (escape-sh-command args) (car args)))))))

(defmethod connection-run ((c ssh-connection) cmd &optional input)
  (multiple-value-bind (out err exit)
      (run :input input (sshcmd c cmd))
    (values (strcat err out) exit)))

(defmethod connection-readfile ((c ssh-connection) path)
  (multiple-value-bind (output error-code)
      (run (sshcmd c "test" "-r" path "&&" "cat" path))
    (if (= 0 error-code)
	output
	(error "File ~S not readable" path))))

(defmethod connection-writefile ((c ssh-connection) path contents)
  (with-remote-temporary-file (temp)
    (run :input contents (sshcmd c "cat" #?">$(temp)"))
    (run "mv" temp path)))

;; rsync it straight to to its destination so rsync can do incremental updates
(defmethod connection-upload ((c ssh-connection) from to)
  (run "rsync" "-Pavc" from (format nil "~A:~A" (ssh-host c) to)))
