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

(defmacro ssh-host ()
  `(if-let ((user (slot-value connection :user)))
     (strcat user "@" (slot-value connection :hostname))
     (slot-value connection :hostname)))

(defmacro sshcmd (&rest args)
  `(list
    "ssh"
    (ssh-host)
    ;; wrap in 'sh -c' in case the login shell is not POSIX
    (strcat "sh -c "
	    (escape-sh-token
	     ,(if (cdr args) `(escape-sh-command ',args) `(car ',args))))))

(defmethod connection-run ((connection ssh-connection) cmd &optional input)
  (run :input input (sshcmd cmd)))

(defmethod connection-readfile ((connection ssh-connection) path)
  (multiple-value-bind (output error-code)
      (run (sshcmd "test" "-r" path "&&" "cat" path))
    (if (= 0 error-code)
	output
	(error "File ~S not readable" path))))

(defmethod connection-writefile ((connection ssh-connection) path contents)
  (with-remote-temporary-file (temp)
    (run :input contents (sshcmd "cat" #?">$(temp)"))
    (run "mv" temp path)))

;; rsync it straight to to its destination so rsync can do incremental updates
(defmethod connection-upload ((connection ssh-connection) from to)
  (run "rsync" "-Pavc" from (strcat (ssh-host) ":" to)))
