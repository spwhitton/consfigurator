(in-package :consfigurator.connection.ssh)

(defmethod establish-connection ((type (eql :ssh)) remaining
				 &key
				   (hop (hostattr *host* :hostname)))
  (declare (ignore remaining))
  (run (shell-cmd "ssh" "-fN" hop))
  (make-instance 'ssh-connection :parent *connection* :hostname hop))

(defclass ssh-connection (posix-connection)
  ((hostname
    :documentation "Hostname to SSH to."))
  (:documentation "Deploy properties using non-interactive SSH."))

(defmethod connection-run ((connection ssh-connection) cmd &optional input)
  ;; wrap in 'sh -c' in case the login shell is not POSIX
  (run (shellcmd "ssh"
		 (slot-value connection :hostname)
		 (shellcmd "sh" "-c" cmd))
       input))
