(in-package :consfigurator.connection.ssh)

(defmethod establish-connection ((type (eql :ssh)) host &key)
  (flet ((ssh-run (shell-cmd &optional input)
	   (assert (stringp shell-cmd))
	   (connection-run
	    ;; wrap in 'sh -c' in case the login shell is not POSIX
	    (shellcmd "ssh" host (shellcmd "sh" "-c" shell-cmd))
	    input)))
    (make-instance 'connection
		   :run #'ssh-run
		   :readfile (lambda (path)
			       (ssh-run (shellcmd "cat" path)))
		   ;; better: write to a temporary file then atomically move
		   ;; into place, in case ssh drops out during the cat ..
		   :writefile (lambda (path contents)
				(ssh-run (shellcmd "cat" :output path)
					 contents))
		   ;; better: rsync it to a temporary location, and then
		   ;; atomically move into place
		   :upload (lambda (from to)
			     (connection-run (shellcmd "scp"
						       from
						       (concat host ":" to))))
		   :teardown #'noop)))
