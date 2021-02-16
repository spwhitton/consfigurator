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
	 (uiop:escape-sh-command "sh" "-c" ,@args)))

(defmethod connection-run ((connection ssh-connection)
			   cmd
			   &optional
			     input
			     environment)
  (run-with-input input environment (sshcmd cmd)))

(defmethod connection-readfile ((connection ssh-connection) path)
  (multiple-value-bind (output error-code)
      (run (sshcmd "test" "-r" "path" "&&" "cat" path))
    (if (= 0 error-code)
	output
	(error "File ~S not readable" path))))

;; write to a temporary file, and then atomically move into place
(defmethod connection-writefile ((connection ssh-connection) path contents))

;; rsync it to a temporary location, and then atomically move into place
(defmethod connection-upload ((connection ssh-connection) from to))
