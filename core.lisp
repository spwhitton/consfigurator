(in-package :consfigurator.core)

;;;; Connections

(defclass connection ()
  ((type
    :initarg :type
    :initform (error "Must supply a type.")
    :documentation "Whether the connection is :posix or :lisp.")
   (run
    :initarg :run
    :initform (error "Must supply a run closure.")
    :documentation "Subroutine to run shell commands on the host.")
   (readfile
    :initarg :readfile
    :initform (error "Must supply a readfile closure.")
    :documentation "Subroutine to read the contents of files on the host.")
   (writefile
    :initarg :writefile
    :initform (error "Must supply a writefile closure.")
    :documentation
    "Subroutine to replace/create the contents of files on the host.")
   (upload
    :initarg :upload
    :initform (error "Must supply an upload closure.")
    :documentation "Subroutine to upload files to the host.")
   (teardown
    :initarg :teardown
    :documentation "Subroutine to disconnect from the host.")))

(defgeneric establish-connection (type host &key)
  (:documentation
   "Within the context of the current connection, connect to a host and return
object representing this new connection."))

(defvar *connection*
  (make-instance
   'connection
   :type :lisp
   :run (lambda (shell-cmd &optional input)
	  (assert (stringp shell-cmd))
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
  "Object representing the currently active connection.
Deployments bind this variable, but its global value should be regarded as a
constant.  This constant value is a connection representing the root
deployment: deploying properties to the machine Lisp is running on.")


;;;; Functions to access the slots of the current connection.

(defun connection-run (cmd &optional input)
  (funcall (slot-value *connection* 'run) cmd input))

(defun connection-readfile (path)
  (funcall (slot-value *connection* 'readfile) path))

(defun connection-writefile (path contents)
  (funcall (slot-value *connection* 'writefile) path contents))

(defun connection-upload (from to)
  (funcall (slot-value *connection* 'upload) from to))


;;;; Properties and property combinators

(defclass property ()
  ((type
    :initarg :type
    :initform (error "Must supply a type.")
    :documentation "Whether the property is :posix or :lisp.")
   (desc
    :initarg :desc
    :documentation "Human-readable description of the property.")
   (hostattrs
    :initarg :hostattrs
    :initform (lambda (&rest args)
		(declare (ignore args))
		nil)
    :documentation
    "Subroutine returning list of host attributes set on hosts with this property.")
   (check
    :initarg :check
    :initform (lambda (&rest args)
		(declare (ignore args))
		(values))
    :documentation "Subroutine to check whether the property is applied.")
   (apply
    :initarg :apply
    :initform (lambda (&rest args)
		(declare (ignore args))
		(values))
    :documentation "Subroutine to apply the property.")
   (unapply
    :initarg :unapply
    :initform (lambda (&rest args)
		(declare (ignore args))
		(values))
    :documentation "Subroutine to unapply the property.")))

;; standard way to write properties is to use this macro, or one of the
;; property combinator functions
(defmacro defprop (name type args &body forms)
  (let ((slots (list :type type)))
    (when (stringp (car forms))
      (setf (getf slots :desc) (pop forms)))
    (loop for form in forms
	  if (keywordp (car form))
	  do (setf (getf slots (car form)) (cdr form)))
    (loop for kw in '(:hostattrs :check :apply :unapply)
	  do (if-let ((slot (getf slots kw)))
	       (setf (getf slots kw)
		     `(lambda ,args ,@slot))))
    `(defparameter ,name
       (make-instance 'property ,@slots)
       ,(getf slots :desc))))
