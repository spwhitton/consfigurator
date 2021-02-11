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
    `(progn
       (declaim (type property ,name))
       (defparameter ,name
	(make-instance 'property ,@slots)
	,(getf slots :desc)))))


;;;; Property application specifications

(defvar *consfig* nil
  "A list of names of the ASDF systems in which you define your hosts,
site-specific properties and deployments.  These systems should depend on the
\"consfigurator\" system.

More specifically, in normal usage of Consfigurator, calling
(mapc #'asdf:require-system *consfig*) should be sufficient to define all the
properties you intend to apply to hosts.

Use the SETCONSFIG macro at the top of your consfig to set this value.")

(defmacro setconsfig (systems)
  "Set the value of *consfig*.  SYSTEMS can be a name or a list of names."
  (when (atom systems)
    (setq systems (list systems)))
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setq *consfig* ',systems)))

(defclass propspec ()
  ((systems
    :initarg :systems
    :documentation "List of names of systems, the loading of all of which is
sufficient to deploy this propspec.")
   (applications
    :initarg :props
    :documentation "Ordered list of property applications.
Each member is of the form (PROPERTY . ARGS) where PROPERTY is a symbol naming
a property (typically as defined by DEFPROP) and ARGS is a list of arguments
to be passed when calling the property's subroutines.  These ARGS will not be
evaluated before calling the function.

Deployments apply properties in the order specified here, so later entries in
the list implicitly depend on earlier ones.

Members of ARGS must all be objects which can be serialised.  In particular,
function objects are not permitted.

The point of this data structure is to be a way to inform a Lisp process
running on a remote host how it can apply some properties: load each of the
systems, and then look in the value cell of each PROPERTY to find a property,
and pass each of ARGS to the function in the property's apply slot.")))

;; for use in macros which need to create property application specifications
;; by evaluating arguments to properties in the current environment
(defun props (applications &optional systems)
  (if systems
      (when (atom systems)
	(setq systems (list systems)))
      (if *consfig*
	  (setq systems (list *consfig*))
	  (error "*consfig* not set")))
  `(make-instance
    'propspec
    :systems ',systems
    :props (list ,@(loop for (prop . args) in applications
			 unless (symbolp prop)
			   do (error "~S is not a symbol" prop)
			 collect `(cons ',prop (mapcar #'eval ',args))))))


;;;; Hosts

(defclass host ()
  ((hostattrs
    :initarg :attrs
    :documentation "Plist of the host's static informational attributes.")
   (properties
    :initarg :props
    :documentation "List of the properties to be applied to the host.")))

(defmacro defhost (hostname &body properties)
  "Define a host with hostname HOSTNAME and properties PROPERTIES.
HOSTNAME can be a string or a symbol.  In either case, the host will get a
static informational property with its hostname as a string, and the symbol
whose name is the hostname will be bound to the host object.

If the first entry in PROPERTIES is a string, it will be considered a
human-readable description of the host."
  (let (hostname-sym hostattrs)
    (etypecase hostname
      (string (setq hostname-sym (intern hostname)))
      (symbol (setq hostname-sym hostname
		    hostname (string-downcase (symbol-name hostname)))))
    (setf (getf hostattrs :hostname) hostname)
    (when (stringp (car properties))
      (setf (getf hostattrs :desc) (pop properties)))
    `(progn
       (declaim (type host ,hostname-sym))
       (defparameter ,hostname-sym
	 (make-instance 'host :attrs ',hostattrs :props ',properties)
	 ,(getf hostattrs :desc)))))


;;;; Lisp systems defining host configurations

(defun get-path-to-concatenated-system (system)
  "Try to concatenate all the source code for SYSTEM, store in the cache and
return the filename.  If asdf doesn't know about SYSTEM, see if we have a
concatenation of all the source code in the cache -- this will typically be
the case when the current Lisp process was launched by a :lisp connection
initiated on another host."
  (let* ((cache-dir (concat (or (uiop:getenv "XDG_CACHE_HOME")
				(concat (uiop:getenv "HOME") "/.cache"))
			    "/consfigurator/systems"))
	 ;; TODO encode -- probably want something like propellor's
	 ;; File.configFileName defined somewhere
	 (cache-file (concat cache-dir
			     "/"
			     (string-downcase (symbol-name system))
			     ".lisp"))
	 (op 'asdf:monolithic-concatenate-source-op)
	 (co (asdf:find-component system nil)))
    (asdf:initialize-output-translations
     `(:output-translations
       (t ,(uiop:parse-unix-namestring cache-dir :type :directory))
       :disable-cache
       :ignore-inherited-configuration))
    (when (asdf:find-system system nil)
      (asdf:operate op co)
      (rename-file (asdf:output-file op co) cache-file))
    (or (probe-file cache-file)
	(error "Could not find system to concatenate"))))
