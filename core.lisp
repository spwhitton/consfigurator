(in-package :consfigurator.core)

;;;; Connections

(defclass connection ()
  ((type
    :initarg :type
    :initform (error "Must supply a type.")
    :documentation "Whether the connection is :posix or :lisp.")

   ;; er, should each of these rather be a generic function?
   (run
    :initarg :run
    :initform (error "Must supply a run closure.")
    :documentation "Subroutine to run shell commands on the host.")
   (readfile
    :initarg :readfile
    :initform (error "Must supply a readfile closure.")
    :documentation "Subroutine to read the contents of files on the host.")
   ;; only functional difference between writefile and upload is what args
   ;; they take: a string vs. a path.  they may have same or different
   ;; implementations
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

(defvar *connection* nil
  "Object representing the currently active connection.
Deployments bind this variable.  Its global value should remain nil.")

(defvar *host* nil
  "Object representing the host to which we're currently connected.
Deployments bind this variable.  Its global value should remain nil.

The main point of this is to allow properties to read the static informational
attributes of the host to which they're being applied.")


;;;; Functions to access the slots of the current connection.

(defun connection-run (cmd &optional input)
  (funcall (slot-value *connection* 'run) cmd input))

(defun connection-readfile (path)
  (funcall (slot-value *connection* 'readfile) path))

(defun connection-writefile (path contents)
  (funcall (slot-value *connection* 'writefile) path contents))

(defun connection-upload (from to)
  (funcall (slot-value *connection* 'upload) from to))


;;;; Properties

;; Properties are not stored as CLOS objects (or structs) in value cells
;; because they are immutable -- see "Attempting to work with anonymous
;; properties or connection types" in the docs.  An alternative would be to
;; use the function cell to store a function which takes
;; 'apply/'hostattrs/etc. as its first argument and dispatches, but those
;; could be flet, which is forbidden.  A determined user could of course edit
;; the symbol plist entries, but we want to make it difficult for someone who
;; hasn't read the docs to accidentally violate immutability

(defun setprop (sym type &key args desc hostattrs check apply unapply)
  ;; use non-keyword keys to avoid clashes with other packages
  (when type
    (setf (get sym 'type) type))
  (when desc
    (setf (get sym 'desc) desc))
  (when hostattrs
    (setf (get sym 'hostattrs) hostattrs))
  (when check
    (setf (get sym 'check) check))
  (when apply
    (setf (get sym 'apply) apply))
  (when unapply
    (setf (get sym 'unapply) unapply))
  sym)

(defun proptype (prop)
  (get prop 'type))

(defun propdesc (prop)
  (get prop 'desc))

(defun propargs (prop)
  (get prop 'args))

(defun propattrs (prop &rest args)
  (when-let ((f (get prop 'hostattrs)))
    (apply f args)))

(defun propcheck (prop &rest args)
  (apply (get prop 'check (lambda (&rest args)
			    (declare (ignore args))
			    (values)))
	 args))

(defun propapply (prop &rest args)
  (apply (get prop 'apply (lambda (&rest args)
			    (declare (ignore args))
			    (values)))
	 args))

(defun propunapply (prop &rest args)
  (apply (get prop 'unapply (lambda (&rest args)
			      (declare (ignore args))
			      (values)))
	 args))

;;; standard way to write properties is to use one of these two macros

(defmacro defprop (name type args &body forms)
  (let ((slots (list :args args)))
    (when (stringp (car forms))
      (setf (getf slots :desc) (pop forms)))
    (loop for form in forms
	  if (keywordp (car form))
	  do (setf (getf slots (car form)) (cdr form)))
    (loop for kw in '(:hostattrs :check :apply :unapply)
	  do (if-let ((slot (getf slots kw)))
	       (setf (getf slots kw)
		     `(lambda ,args ,@slot))))
    `(setprop ',name ,type ,@slots)))

(defmacro defproplist (name args &body propspec)
  "Define a property which applies a property application specification.")


;;;; Property application specifications

(defvar *consfig* nil
  "A list of names of the ASDF systems in which you define your hosts,
site-specific properties and deployments.  These systems should depend on the
\"consfigurator\" system.

More specifically, in normal usage of Consfigurator, calling
(mapc #'asdf:require-system *consfig*) should be sufficient to define all the
properties you intend to apply to hosts.

Use the SETCONSFIG macro at the top of your consfig to set this value.

Note that you can use Consfigurator without setting this variable, by
explicitly specifying the names of systems when creating property application
specifications.  This is useful if you have more than one consfig that you
want to keep completely independent of each other.")

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
The base case valid entry is of the form (PROPERTY . ARGS) where PROPERTY is
a symbol naming a property (typically as defined by DEFPROP) and ARGS is a
list of arguments to be passed when calling the property's subroutines.  These
ARGS will not be evaluated before calling the function.

Additionally, entries can be of the following forms:

    (unapply (PROPERTY . ARGS)) -- unapply the property, if it supports that.

    ((PROPERTY . ARGS) onchange (PROPERTY . ARGS) onchange (PROPERTY . ARGS))
    -- apply the second and third properties in the case that the first
       property actually had work to do.

... and combinations thereof.

Deployments apply properties in the order specified here, so later entries in
the list implicitly depend on earlier ones.

Members of ARGS must all be objects which can be serialised.  In particular,
function objects are not permitted."))
  (:documentation
   "The point of this data structure is to be a way to inform a Lisp process
running on a remote host how it can apply some properties: load each of the
systems, resolve unapply, onchange etc., and then look in the value cell of
each PROPERTY to find a property, and pass each of ARGS to the function in the
property's apply slot."))

;; the following three functions, plus simple concatenation, should be
;; everything we need to do with propspecs, so all knowledge of the possible
;; combinator symbols should be confined to these three functions

;; to implement:
;;
;; (((file:contains-lines '("foo" "bar"))
;;   on-change (apt:installed '("sbcl")))
;;
;;  (unapply (file:exists "/foo/bar"))
;;
;;  (unapply ((foo) onchange (bar)))
;;
;;  ((foo) onchange (unapply (bar))))

(defun eval-propspec (propspec)
  "Apply properties as specified by PROPSPEC."
  (mapc #'asdf:require-system (slot-value propspec 'systems))
  (loop for form in (slot-value propspec 'applications)
	;; do (something)
	))

(defun propspec->hostattrs (propspec)
  "Return all the hostattrs which should be applied to the host which has
PROPSPEC applied.")

(defun props (forms &optional systems)
  "Where FORMS is the elements of a property application specification, except
that the arguments to properties are expressions to be evaluated to produce
the arguments to be passed rather than literal arguments, return code which
will evaluate the expressions and produce the corresponding property
application specification.

SYSTEMS is the 'systems attribute of the property application specification
that the returned code should produce.

Intended for use by macros which allow the user to provide expressions instead
of values as the arguments to properties when building a property application
specification."
  (if systems
      (when (atom systems)
	(setq systems (list systems)))
      (if *consfig*
	  (setq systems (list *consfig*))
	  (error "*consfig* not set")))
  ;; (something)
  )


;;;; Hosts

(defclass host ()
  ((hostattrs
    :initarg :attrs
    :documentation "Plist of the host's static informational attributes.")
   (propspec
    :initarg :props
    :documentation "Property application specification of the properties to
be applied to the host.")))

(defmacro defhost (hostname &body properties)
  "Define a host with hostname HOSTNAME and properties PROPERTIES.
HOSTNAME can be a string or a symbol.  In either case, the host will get a
static informational property with its hostname as a string, and the symbol
whose name is the hostname will be bound to the host object.

If the first entry in PROPERTIES is a string, it will be considered a
human-readable description of the host.

Otherwise, the entries of PROPERTIES are of the form (PROPERTY . ARGS) where
PROPERTY is a symbol which names a property.  PROPERTIES will be converted
into a property application specification by evaluating each of ARGS in the
current environment.

The order of PROPERTIES matters: deployments will apply properties to the host
in the order specified here, so later properties implicitly depend on earlier
ones.  In addition, static informational attributes set by later properties
are allowed to override any attributes with the same name set by earlier
entries."
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
	 (let* ((propspec ,(props properties))
		(hostattrs (nconc (propspec->hostattrs propspec)
				  ',hostattrs)))
	   (make-instance 'host :attrs hostattrs :props propspec))
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
	 (cache-file (concat cache-dir
			     "/"
			     (string->filename
			      (string-downcase (symbol-name system)))
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
