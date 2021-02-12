(in-package :consfigurator.connection.local)

(defconstant local-connection
  (make-instance
   'connection
   :type :lisp
   :run (lambda (shell-cmd &optional input)
	  (declare (ftype (function (string string) (values string integer))))
	  ;; assumes a POSIX shell (otherwise we could wrap in 'sh -c')
	  (multiple-value-bind (output _ exit-code)
	      (uiop:run-program shell-cmd
				:force-shell t
				:input (and input
					    (make-string-input-stream input))
				:output :string
				:error-output :output)
	    (declare (ignore _))
	    (values output exit-code)))
   :readfile (lambda (path)
	       (uiop:read-file-string path))
   :writefile (lambda (path contents)
		(with-open-file (stream
				 path
				 :direction :output
				 :if-exists :supersede)
		  (write-string contents stream)))
   :upload #'uiop:copy-file
   :teardown #'noop)
  "Object representing the root deployment: applying properties to the
machine the root Lisp is running on, as the root Lisp's uid.")
