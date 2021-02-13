(in-package :consfigurator.core)

;;;; Connections

;; generic function operating on keywords which identify connection types
(defgeneric connect-and-apply (type host &key)
  (:documentation
   "Within the context of the current connection, connect to HOST by
establishing a new connection of type TYPE, and apply HOST's properties.

Implementations of this function either instantiate a connection object and
pass that to APPLY-PROPERTIES, or start up another Lisp process somewhere and
have it call APPLY-PROPERTIES by means of a :local connection."))

(defun apply-properties (connection host)
  "In the context of CONNECTION, apply HOST's properties to HOST.

This function is called by implementations of CONNECT-AND-APPLY."
  (let ((*host* host)
	(*connection* connection))
    (eval-propspec (slot-value host 'propspec))
    (connection-teardown connection)))

(defclass connection ()
  ((parent
    :init-arg :parent
    :documentation
    "The value of *CONNECTION* at the time this connection was established.")))

(defclass lisp-connection (connection))

(defclass posix-connection (connection))

;;; generic functions to operate on subclasses of CONNECTION

(defgeneric connection-run (connection cmd &optional input)
  (:documentation "Subroutine to run shell commands on the host."))

(defgeneric connection-readfile (connection path)
  (:documentation "Subroutine to read the contents of files on the host."))

;; only functional difference between writefile and upload is what args they
;; take: a string vs. a path.  they may have same or different implementations

(defgeneric connection-writefile (connection path contents)
  (:documentation
   "Subroutine to replace/create the contents of files on the host."))

(defgeneric connection-upload (connection from to)
  (:documentation "Subroutine to upload files to the host."))

(defgeneric connection-teardown (connection)
  (:documentation "Subroutine to disconnect from the host."))

;; many connection types don't need anything to be done to disconnect
(defmethod connection-teardown (&rest args)
  (declare (ignore args))
  (values))

(defmacro defconnmethod (name args &body body)
  `(defmethod ,name ,args
     (let ((*connection* (slot-value ,(caar args) 'parent)))
       ,@body)))

;; global value gets set in connection/local.lisp, but the symbol is not
;; exported as it should only get bound by APPLY-PROPERTIES
(defvar *connection* nil
  "Object representing the currently active connection.
Connections dynamically bind this variable and then apply properties.  Its
global value be regarded as a constant.")

(defvar *host* nil
  "Object representing the host to which we're currently connected.
Deployments bind this variable.  Its global value should remain nil.

The main point of this is to allow properties to read the static informational
attributes of the host to which they're being applied.")


;;;; Functions to access the slots of the current connection

;; used by properties and by implementations of CONNECT-AND-APPLY

(defun run (&rest args)
  (apply #'connection-run *connection* args))

(defun readfile (&rest args)
  (apply #'connection-readfile *connection* args))

(defun writefile (&rest args)
  (apply #'connection-writefile *connection* args))

(defun upload (&rest args)
  (apply #'connection-upload *connection* args))


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

(defun propapptype (propapp)
  (get (car propapp) 'type))

(defun collapse-types (&rest lists)
  (if (some (lambda (type) (eq type :posix))
	    (flatten lists))
      :posix
      :lisp))

(defun propdesc (prop)
  (get prop 'desc))

(defun propargs (prop)
  (get prop 'args))

(defun propattrs (prop &rest args)
  (when-let ((f (get prop 'hostattrs)))
    (apply f args)))

(defun propappattrs (propapp)
  (apply #'propattrs (car propapp) (cdr propapp)))

(defun propcheck (prop &rest args)
  (apply (get prop 'check (lambda (&rest args)
			    (declare (ignore args))
			    (values)))
	 args))

(defun propappcheck (propapp)
  (apply #'propcheck (car propapp) (cdr propapp)))

(defun propapply (prop &rest args)
  (apply (get prop 'apply (lambda (&rest args)
			    (declare (ignore args))
			    (values)))
	 args))

(defun propappapply (propapp)
  (apply #'propapply (car propapp) (cdr propapp)))

(defun propunapply (prop &rest args)
  (apply (get prop 'unapply (lambda (&rest args)
			      (declare (ignore args))
			      (values)))
	 args))

(defun propappunapply (propapp)
  (apply #'propunapply (car propapp) (cdr propapp)))

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

;; The following four functions, plus simple concatenation, should be
;; everything we need to do with propspecs, so all knowledge of the possible
;; combinator symbols should be confined to these four functions -- i.e., if
;; we are to add any combinators, this is the code that needs to change

(defun compile-propapp (propapp)
  "Recursively apply the effects of property combinators in PROPAPP to produce
an atomic property application."
  (let ((sym (gensym)))
    (cond
      ;; UNAPPLY
      ((symbol-named unapply (car propapp))
       (destructuring-bind (psym . args) (compile-propapp (cadr propapp))
	 (setprop sym (proptype psym)
		  :desc (concat "Unapply: " (propdesc psym))
		  :check (complement (get psym 'check))
		  :apply (get psym 'unapply)
		  :unapply (get psym 'apply))
	 (cons sym args)))
      ;; ON-CHANGE
      ;; Following pretty much assumes that on-change is our only infix
      ;; property combinator.
      ((symbol-named on-change (cadr propapp))
       (let ((propapps (loop with remaining = (cdr propapp)
			     with apps
			     for s = (pop remaining)
			     for a = (pop remaining)
			     unless (symbol-named on-change s)
			       do (error "Invalid on-change expression")
			     else
			       do (push (compile-propapp a) apps)
			     unless remaining return apps)))
	 (destructuring-bind (psym . args) (compile-propapp (car propapp))
	   (setprop sym (collapse-types (proptype psym)
					(mapcar #'propapptype propapps))
		    :desc (propdesc psym)
		    :hostattrs (lambda (&rest args)
				 (nconc (apply #'propattrs psym args)
					(mapcan #'propappattrs propapps)))
		    :check (get psym 'check)
		    :apply (lambda (&rest args)
			     (unless (eq :nochange
					 (apply #'propapply psym args))
			       (loop for propapp in propapps
				     do (unless (propappcheck propapp)
					  (propappapply propapp)))))
		    :unapply (lambda (&rest args)
			       (unless (eq :nochange
					   (apply #'propunapply psym args))
				 (loop for propapp in propapps
				       do (unless (propappcheck propapp)
					    (propappapply propapp))))))
	   (cons sym args))))
      ;; atomic property application
      (t
       propapp))))

(defun eval-propspec (propspec)
  "Apply properties as specified by PROPSPEC."
  (mapc #'asdf:require-system (slot-value propspec 'systems))
  (loop for form in (slot-value propspec 'applications)
	for propapp = (compile-propapp form)
	do (unless (propappcheck propapp)
	     (propappapply propapp))))

(defun propspec->hostattrs (propspec)
  "Return all the hostattrs which should be applied to the host which has
PROPSPEC applied."
  ;; we need to reverse the plist because hostattrs set by later entries in
  ;; the propspec should override hostattrs set by earlier entries
  (do* ((hostattrs (loop for form in (slot-value propspec 'applications)
			 for propapp = (compile-propapp form)
			 nconc (propappattrs propapp)))
	reversed
	(k (pop hostattrs) (pop hostattrs))
	(v (pop hostattrs) (pop hostattrs)))
       ((not k) reversed)
    (push v reversed)
    (push k reversed)))

(defun props (forms &optional systems)
  "Where FORMS is the elements of an unevaluated property application
specification, return code which will evaluate the expressions and produce the
corresponding property application specification.

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
  (labels ((make-eval-propspec (form)
	     (if (atom form)
		 `(quote ,form)
		 (destructuring-bind (first . rest) form
		   (if (and (symbolp first)
			   (not (member (symbol-name first)
					'("UNAPPLY")
					:test #'string=)))
		       ;; TODO this eval is not what we want because we want
		       ;; the lexical environment in which the macro call
		       ;; occurs, not the null lexical environment!
		       `(cons ',first (mapcar #'eval ',rest))
		       `(list ,@(mapcar #'make-eval-propspec form)))))))
    `(make-instance
      'propspec
      :systems ',systems
      :props (list ,@(mapcar #'make-eval-propspec forms)))))


;;;; Hosts

(defclass host ()
  ((hostattrs
    :initarg :attrs
    :documentation "Plist of the host's static informational attributes.")
   (propspec
    :initarg :props
    :documentation "Property application specification of the properties to
be applied to the host.")))

(defun hostattr (host key)
  "Retrieve a single static informational attribute."
  (getf (slot-value host 'hostattrs) key))

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


;;;; Deployments

(defmacro defdeploy (name (connection host) &body additional-properties)
  "Define a function which does (DEPLOY CONNECTION HOST ADDITIONAL-PROPERTIES).
You can then eval (NAME) to execute this deployment."
  `(defun ,name ()
     (deploy ,connection ,host ,@additional-properties)))

(defmacro defdeploy-these (name (connection host) &body properties)
  "Define a function which does (DEPLOY-THESE CONNECTION HOST PROPERTIES).
You can then eval (NAME) to execute this deployment."
  `(defun ,name ()
     (deploy-these ,connection ,host ,@properties)))

(defmacro defhostdeploy (connection host-name)
  "Where HOST-NAME names a host as defined with DEFHOST, define a function
which does (deploy CONNECTION (symbol-value HOST)).
You can then eval (HOST-NAME) to execute this deployment.

For example, if you usually deploy properties to athena by SSH,

    (defhost athena.silentflame.com
      (foo)
      (bar)
      ...)

    (defhostdeploy :ssh athena.silentflame.com)

and then you can eval (athena.silentflame.com) to apply athena's properties."
  `(defdeploy ,host-name (,connection ,host-name)))

(defmacro deploy (connection host &body additional-properties)
  "Establish a connection of type CONNECTION to HOST, and apply each of the
host's usual properties, followed by specified by ADDITIONAL-PROPERTIES, an
unevaluated property application specification.

CONNECTION is either a keyword identifying a connection type, or a list
beginning with such a keyword and followed by keyword arguments required to
establish the connection.

Then HOST has all its usual static informational attributes, plus any set by
ADDITIONAL-PROPERTIES.  Static informational attributes set by
ADDITIONAL-PROPERTIES can override the host's usual static informational
attributes, in the same way that later entries in the list of properties
specified in DEFHOST forms can override earlier entries (see DEFHOST's
docstring)."
  (with-gensyms (propspec)
    `(let ((,propspec ,(props additional-properties)))
       (deploy* ,connection
		(make-instance 'host
			       :attrs (nconc (propspec->hostattrs ,propspec)
					     (slot-value ,host 'hostattrs))
			       :props (append (slot-value ,host 'propspec)
					      ,propspec))))))

(defmacro deploy-these (connection host &body properties)
  "Establish a connection of type CONNECTION to HOST, and apply each of
the properties specified by PROPERTIES, an unevaluated property application
specification (and not the host's usual properties, unless they also appear
in PROPERTIES).

CONNECTION is either a keyword identifying a connection type, or a list
beginning with such a keyword and followed by keyword arguments required to
establish the connection.

This function is useful to apply one or two properties to a host right now,
e.g. at the REPL when when testing new property definitions.  If HOST is
usually deployed using a :lisp connection, and the property you are testing
is :posix, you might use a connection type like :ssh so that you can quickly
alternate between redefining your work-in-progress property and attempting to
apply it to HOST.

HOST has all its usual static informational attributes, as set by its usual
properties, plus any set by PROPERTIES.  Static informational attributes set
by PROPERTIES can override the host's usual static informational attributes,
in the same way that later entries in the list of properties specified in
DEFHOST forms can override earlier entries (see DEFHOST's docstring)."
  (with-gensyms (propspec)
    `(let ((,propspec ,(props properties)))
       (deploy* ,connection
		(make-instance 'host
			       :attrs (nconc (propspec->hostattrs ,propspec)
					     (slot-value ,host 'hostattrs))
			       :props ,propspec)))))


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
