(in-package :consfigurator.core)

;;;; Connections

;; generic function operating on keywords which identify connection types
(defgeneric establish-connection (type remaining &key)
  (:documentation
   "Within the context of the current connection, connect to HOST by establishing
a new connection of type TYPE.
Either starts a Lisp process somewhere else, tells it to continue establishing
REMAINING (by telling it to call DEPLOY* with arguments obtained by (locally)
evaluating, on our side of the connection,
(list (or REMAINING '(:local)) *host*)), and returns nil, or returns a object
suitable for *connection*.

Any implementation which hands over to a remote Lisp process will need to
upload any prerequisite data required by the deployment."))

(defclass connection ()
  ((parent
    :initform *connection*
    :documentation
    "The value of *CONNECTION* at the time this connection was established.")))

(defclass lisp-connection (connection) ())

(defclass posix-connection (connection) ())

;;; generic functions to operate on subclasses of CONNECTION

(defgeneric connection-run (connection cmd &optional input environment)
  (:documentation "Subroutine to run shell commands on the host."))

(defmethod connection-run :around ((connection connection)
				   cmd
				   &optional
				     input
				     environment)
  (declare (ignore input environment))
  (let ((*connection* (slot-value connection 'parent)))
    (call-next-method)))

(defgeneric connection-readfile (connection path)
  (:documentation "Subroutine to read the contents of files on the host."))

(defmethod connection-readfile :around ((connection connection) path)
  (let ((*connection* (slot-value connection 'parent)))
    (call-next-method)))

;; only functional difference between writefile and upload is what args they
;; take: a string vs. a path.  they may have same or different implementations

(defgeneric connection-writefile (connection path contents)
  (:documentation
   "Subroutine to replace/create the contents of files on the host."))

(defmethod connection-writefile :around ((connection connection) path contents)
  (let ((*connection* (slot-value connection 'parent)))
    (call-next-method)))

(defgeneric connection-upload (connection from to)
  (:documentation "Subroutine to upload files to the host."))

(defmethod connection-upload :around ((connection connection) from to)
  (let ((*connection* (slot-value connection 'parent)))
    (call-next-method)))

(defgeneric connection-teardown (connection)
  (:documentation "Subroutine to disconnect from the host."))

(defmethod connection-teardown :around ((connection connection))
  (let ((*connection* (slot-value connection 'parent)))
    (call-next-method)))

;; many connection types don't need anything to be done to disconnect
(defmethod connection-teardown ((connection connection))
   (values))

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

;; used by properties and by implementations of ESTABLISH-CONNECTION

(defun run (&rest args)
  (funcall #'connection-run
	   *connection*
	   (if (cdr args) (uiop:escape-sh-command args) args)))

(defun run-with-input (input environment &rest args)
    (funcall #'connection-run
	   *connection*
	   (if (cdr args) (uiop:escape-sh-command args) args)
	   input
	   environment))

(defun runlines (&rest args)
  (unlines (apply #'run args)))

(defun runlines-with-input (&rest args)
  (unlines (apply #'run-with-input args)))

(defun readfile (&rest args)
  (apply #'connection-readfile *connection* args))

(defun writefile (&rest args)
  (apply #'connection-writefile *connection* args))


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
  (apply (get prop 'hostattrs (lambda (&rest args)
				(declare (ignore args))
				(values)))
	 args))

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

;; TODO when forms is not (:apply etc.) but just code, we could just consider
;; that all to be :apply, and leave :hostattrs, :check and :unapply blank?
;; TODO :push-hostattrs to specify a function which does not look at
;; *hostattrs* and just returns a list which gets added to the front (we will
;; wrap (push ... *hostattrs*) around the return value, basically)
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
		     ;; inside this lambda we could do some checking of, e.g.,
		     ;; whether we are :lisp but this connection is
		     ;; posix-connection.  possibly a condition with a restart
		     ;; which allows skipping over this property
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
		  :desc (strcat "Unapply: " (propdesc psym))
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
	unless (propappcheck propapp)
	  do (propappapply propapp)))

(defun propspec->hostattrs (propspec)
  "Return all the hostattrs which should be applied to the host which has
PROPSPEC applied."
  (loop with *hostattrs*
	for form in (slot-value propspec 'applications)
	for propapp = (compile-propapp form)
	do (propappattrs propapp)
	finally (return *hostattrs*)))

(defun propspec->type (propspec)
  "Return :lisp if any types of the properties to be applied by PROPSPEC is
:lisp, else return :posix."
  (loop for form in (slot-value propspec 'applications)
	for propapp = (compile-propapp form)
	if (eq (propapptype propapp) :lisp)
	  return :lisp
	finally (return :posix)))

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
      ;; TODO maybe setconsfig could set *consfig* just within the current
      ;; package and then macros bind it or pass it to this function.  then no
      ;; global value, i.e. drop that piece of state.
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
		       `(list ',first ,@rest)
		       `(list ,@(mapcar #'make-eval-propspec form)))))))
    `(make-instance
      'propspec
      :systems ',systems
      :props (list ,@(mapcar #'make-eval-propspec forms)))))

;;; property :hostattrs subroutines

(defvar *hostattrs* nil
  "Used by property :hostattrs subroutines, only, to access and modify the
current static informational attributes, and to add new ones.")

(defun add-hostattr (k v)
  (push *hostattrs* v)
  (push *hostattrs* k))

(defun require-data (iden1 iden2)
  (push (getf *hostattrs* :data) (cons iden1 iden2)))


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
human-readable description of the host.  Otherwise, PROPERTIES is an
unevaluated property application specification.  Recall that for atomic
entries (PROPERTY . ARGS), PROPERTY refers to the property that symbol names
in the global environment, not whatever it may name in the current dynamic
and/or lexical environments.  Property application specifications cannot
close over globally anonymous properties.

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
  (once-only (host)
    (with-gensyms (propspec)
      `(let ((,propspec ,(props additional-properties)))
	 (deploy* ,connection
		  (make-instance 'host
				 :attrs (nconc (propspec->hostattrs ,propspec)
					       (slot-value ,host 'hostattrs))
				 :props (append (slot-value ,host 'propspec)
						,propspec)))))))

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

(defun deploy* (connections host)
  (let ((*host* host))
    (labels
	((connect (connections)
	   (destructuring-bind ((type . args) . remaining) connections
	     (when-let ((*connection*
			 (apply #'establish-connection type remaining args)))
	       (if remaining
		   (connect remaining)
		   (apply-propspec (slot-value *host* 'propspec)))
	       (connection-teardown *connection*))))
	 (apply-propspec (propspec)
	   (when (and (subtypep (class-of *connection*) 'posix-connection)
		      (eq :lisp (propspec->type propspec)))
	     (error "Cannot apply :lisp properties using :posix connection"))
	   (eval-propspec propspec)))
      (connect (loop for connection in (ensure-cons connections)
		     collect (ensure-cons connection))))))

(defprop deploy :posix (connection host &rest additional-properties)
  "Execute a Consfigurator deployment.

Useful to have one host act a controller, applying properties to other hosts.
Also useful to set up VMs, chroots, disk images etc. on localhost.")

(defprop deploy-these :posix (connection host &rest properties)
  "Execute a deployment, but replace the properties of host with PROPERTIES.
This property is to the DEPLOY property what the DEPLOY-THESE function is to
the DEPLOY function.")


;;;; Prerequisite data

(defvar *data-sources* nil "Known sources of prerequisite data.")

(defun add-data-source (check provide)
  (push (cons check provide) *data-sources*))

;; if this proves to be inadequate then an alternative would be to maintain a
;; mapping of ASDF systems to data sources, and then APPLY-PROPERTIES could
;; look up the data sources registered for the systems in (slot-value
;; (slot-value host 'propspec) 'systems) and bind *data-sources* to point to
;; those just how it binds *host* and *connection*.  registering a source
;; means registering it in the mapping of systems to sources
(defgeneric register-data-source (type &key)
  (:documentation
   "Initialise and register a source of prerequisite data in this Lisp process.
Registered data sources are available to all deployments executed from the
root Lisp, regardless of the consfig which defines the host to which
properties are to be applied.  (This could only cause problems if you have
different consfigs with prerequisite data which is identified by the same two
strings, in which case you will need to wrap your deployments with registering
and unregistering data sources.  Usually items of prerequisite data are
identified using things like hostnames, so this is unlikely to be necessary.)

Implementation of this function call ADD-DATA-SOURCE, providing two functions.

Signals a condition MISSING-DATA-SOURCE when unable to access the data source
(e.g. because can't decrypt it).  This condition is captured and ignored in
all Lisp processes started up by Consfigurator, since prerequisite data
sources are not expected to be available outside of the root Lisp."))

(defprop data-uploaded :posix (iden1 iden2 &optional destination)
    ;; calls get-data
    )

(defprop host-data-uploaded :posix (destination)
  (:apply (propapply 'data-uploaded
		     (hostattr *host* :hostname)
		     destination
		     destination)))

(defun get-data (iden1 iden2)
  (if-let ((source-thunk (cdr (query-data-sources iden1 iden2))))
    (funcall source-thunk)
    ;; now look in local cache -- note that this won't exist in the root Lisp,
    ;; but only if we're a Lisp started up by a connection

    ))

(defun query-data-sources (iden1 iden2)
  (car (sort (loop for (ver . get) in *data-sources*
		   for version = (funcall ver iden1 iden2)
		   when version collect (cons version
					      (lambda ()
						(funcall get iden1 iden2))))
	     (compose #'version> #'car))))

;; called by implementations of ESTABLISH-CONNECTION which start up remote
;; Lisp processes
(defun upload-all-prerequisite-data (host)
  (loop with *data-sources*
	initially (register-data-source :asdf)

	with sorted-local-cache  = (sort (get-local-cached-prerequisite-data)
					 (compose #'version> #'third))
	with sorted-remote-cache = (sort (get-remote-cached-prerequisite-data)
					 (compose #'version> #'third))

	for (iden1 . iden2) in (getf (slot-value host :hostattrs) :data)
	for highest-local-cached-version
	  = (third (car (remove-if-not (lambda (c)
					 (and (string= (first c) iden1)
					      (string= (second c) iden2)))
				       sorted-local-cache)))
	for highest-remote-cached-version
	  = (third (car (remove-if-not (lambda (c)
					 (and (string= (first c) iden1)
					      (string= (second c) iden2)))
				       sorted-remote-cache)))
	for (highest-source-version . highest-source)
	  = (query-data-sources iden1 iden2)

	if (and highest-source-version
		(or (not highest-remote-cached-version)
		    (version< highest-remote-cached-version
			      highest-source-version)))
	  do (connection-clear-data-cache iden1 iden2)
	     (connection-upload-data iden1
				     iden2
				     highest-source-version
				     (funcall highest-source))
	else if (and highest-local-cached-version
		     (or (not highest-remote-cached-version)
			 (version< highest-remote-cached-version
				   highest-local-cached-version)))
	       do (connection-clear-data-cache iden1 iden2)
		  (connection-upload-data
		   iden1
		   iden2
		   highest-local-cached-version
		   (list :file
			 (local-data-pathname iden1
					      iden2
					      highest-local-cached-version)))
	else if (not highest-remote-cached-version)
	       do (error "Could not provide prerequisite data ~S | ~S"
			 iden1 iden2)))

(defun local-data-pathname (&rest segments)
  (reduce #'merge-pathnames (nreverse (mapcar #'string->filename segments))
	  :from-end t :initial-value (get-local-data-cache-dir)))

(defun remote-data-pathname (&rest segments)
  (reduce #'merge-pathnames (nreverse (mapcar #'string->filename segments))
	  :from-end t :initial-value (get-remote-data-cache-dir)))

(defun connection-upload-data (iden1 iden2 version data)
  (let* ((dest (remote-data-pathname iden1 iden2 version)))
    (run "mkdir" "-p" (uiop:unix-namestring
		       (uiop:pathname-directory-pathname dest)))
    (cond
      ((getf data :file)
       ;; TODO if (string-prefix-p "text/" (getf data :mime)) then gzip,
       ;; upload and gunzip
       (connection-upload *connection*
			  (uiop:unix-namestring (getf data :file))
			  dest))
      ((getf data :data)
       (connection-writefile *connection* dest (getf data :data)))
      (t
       (error "Prerequisite data plist lacks both :file and :data entries")))))

(defun connection-clear-data-cache (iden1 iden2)
  (let ((dir (uiop:ensure-directory-pathname
	      (remote-data-pathname iden1 iden2))))
    (run "rm" "-f" (strcat (uiop:unix-namestring
			    (uiop:pathname-directory-pathname dir))
			   "/*"))))

(defun get-local-data-cache-dir ()
  (uiop:ensure-directory-pathname
   (strcat (or (uiop:getenv "XDG_CACHE_HOME")
	       (strcat (uiop:getenv "HOME") "/.cache"))
	   "/consfigurator/data")))

(defun get-local-cached-prerequisite-data ()
  "Return a list of items of prerequisite data in the cache local to this Lisp
process, where each entry is of the form

    '(iden1 iden2 version)."
  (loop for dir in (uiop:subdirectories (get-local-data-cache-dir))
	nconc (loop for subdir in (uiop:subdirectories dir)
		    nconc (loop for file in (uiop:directory-files subdir)
				collect (mapcar #'filename->string
						(list dir subdir file))))))

(defun get-remote-data-cache-dir ()
  (uiop:ensure-directory-pathname
   (car
    (runlines "echo" "${XDG_CACHE_HOME:-$HOME/.cache}/consfigurator/data/"))))

(defun get-remote-cached-prerequisite-data ()
  "Return a list of items of prerequisite data in the cache on the remote side
of the current connection, where each entry is of the form

    '(iden1 iden2 version)."
  (mapcar (lambda (line)
	    (mapcar #'filename->string (split-string line :separator "/")))
	  (runlines "find" (get-remote-data-cache-dir)
		    "-type" "f" "-printf" "%P\\n")))
