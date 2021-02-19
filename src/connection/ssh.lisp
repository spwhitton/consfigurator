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

(defmethod establish-connection ((type (eql :ssh)) remaining
				 &key
				   (hop (hostattr *host* :hostname)))
  (declare (ignore remaining))
  (run "ssh" "-fN" hop)
  (make-instance 'ssh-connection :hostname hop))

(defclass ssh-connection (posix-connection)
  ((hostname
    :documentation "Hostname to SSH to."))
  (:documentation "Deploy properties using non-interactive SSH."))

(defmacro sshcmd (&rest args)
  ;; wrap in 'sh -c' in case the login shell is not POSIX
  `(list "ssh"
	 (slot-value connection :hostname)
	 (escape-sh-command "sh" "-c" ,@args)))

(defmethod connection-run ((connection ssh-connection) cmd &optional input)
  (run :input input (sshcmd cmd)))

(defmethod connection-readfile ((connection ssh-connection) path)
  (multiple-value-bind (output error-code)
      (run (sshcmd "test" "-r" "path" "&&" "cat" path))
    (if (= 0 error-code)
	output
	(error "File ~S not readable" path))))

;; write to a temporary file, and then atomically move into place
(defmethod connection-writefile ((connection ssh-connection) path contents))

;; rsync it to its destination, so rsync can be smart about updates
(defmethod connection-upload ((connection ssh-connection) from to))
