(in-package :consfigurator.connection.local)

(defmethod establish-connection ((type (eql :local)) host &key)
  (make-instance 'local-connection))

(defclass local-connection (lisp-connection)
  ()
  (:documentation "The root deployment: applying properties to the machine the
root Lisp is running on, as the root Lisp's uid."))

(defmethod connection-run ((connection local-connection)
			   shell-cmd
			   &optional
			     input)
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

(defmethod connection-readfile ((connection local-connection) path)
  (uiop:read-file-string path))

(defmethod connection-writefile ((connection local-connection) path contents)
  (with-open-file (stream path :direction :output :if-exists :supersede)
    (write-string contents stream)))

(defmethod connection-upload ((connection local-connection) from to)
  (uiop:copy-file from to))

;; set the root Lisp's connection context now we've defined its value -- other
;; implementations of ESTABLISH-CONNECTION will rely on this when they call
;; RUN, READFILE etc.
(eval-when (:load-toplevel :execute)
  (unless consfigurator.core::*connection*
    (setq consfigurator.core::*connection*
	  (make-instance 'local-connection))))
