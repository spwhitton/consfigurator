(in-package :consfigurator.connection.ssh)

(defmethod connect-and-apply ((type (eql :ssh)) host &key)
  (run (shell-cmd "ssh" "-fN" (hostattr host :hostname)))
  (apply-properties (make-instance 'ssh-connection :parent *connection*) host))

(defclass ssh-connection (posix-connection)
  ()
  (:documentation "Deploy properties using non-interactive SSH."))

(defconnmethod connection-run ((connection ssh-connection) cmd &optional input)
  ;; wrap in 'sh -c' in case the login shell is not POSIX
  (run (shellcmd "ssh" (hostattr *host* :hostname) (shellcmd "sh" "-c" cmd))
       input))
