;;; Consfigurator -- Lisp declarative configuration management system

;;; Copyright (C) 2021  Sean Whitton <spwhitton@spwhitton.name>

;;; This file is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3, or (at your option)
;;; any later version.

;;; This file is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.

;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(in-package :consfigurator)

;;;; Property application specifications

(defmacro in-consfig (systems)
  "Sets the variable *CONSFIG* in the current package to SYSTEMS, or (SYSTEMS)
if SYSTEMS is an atom.  Used at the top of your consfig, right after IN-PACKAGE.

This is used to record a list of the names of the ASDF systems in which you
define your hosts, site-specific properties and deployments.  These systems
should depend on the \"consfigurator\" system.

SYSTEMS should satisfy the following condition: in normal usage of
Consfigurator, evaluating
(mapc #'asdf:load-system (if (atom SYSTEMS) (list SYSTEMS) SYSTEMS) should be
sufficient to define all the properties you intend to apply to hosts.

Consfigurator uses this information when starting up remote Lisp images to
effect deployments: it sends over the ASDF systems specified by SYSTEMS."
  (setq systems (ensure-cons systems))
  (let ((sym (intern "*CONSFIG*")))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (defparameter ,sym ',systems
    "ASDF systems the loading of all of which is sufficient to define all the
Consfigurator properties code in this symbol's package applies to hosts."))))

(defclass propspec ()
  ((systems
    :initarg :systems
    :initform (or (symbol-value (find-symbol "*CONSFIG*"))
		  (error
		   "Looks like *CONSFIG* is not set; please call IN-CONSFIG"))
    :reader propspec-systems
    :documentation "List of names of systems, the loading of all of which is
sufficient to deploy this propspec.")
   (applications
    :initarg :props
    :reader propspec-props
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
   "The point of this data structure is to be a way to inform a Lisp image
running on a remote host how it can apply some properties: load each of the
systems, resolve unapply, onchange etc., and then look in the value cell of
each PROPERTY to find a property, and pass each of ARGS to the function in the
property's apply slot."))

(defun make-propspec (&key (systems nil systems-supplied-p) props)
  (setq props (copy-tree props))
  (labels ((preprocess (item)
	     (cond
	       ((and (listp item) (isprop (car item)))
		(rplacd item (apply (proppp (car item)) (cdr item))))
	       ((consp item)
		(mapc #'preprocess item)))))
    (preprocess props))
  (if systems-supplied-p
      (make-instance 'propspec :props props :systems systems)
      (make-instance 'propspec :props props)))

;; does not use MAKE-PROPSPEC because we do not want the :PREPROCESS
;; subroutines to be run again when the object is read back in
(defmethod print-object ((propspec propspec) stream)
  (format stream "#.~S" `(make-instance
			  'propspec
			  :systems ',(slot-value propspec 'systems)
			  :props ',(slot-value propspec 'applications)))
  propspec)


;; doesn't use MAKE-PROPSPEC because each of the propspecs will already have
;; had its :PREPROCESS subroutines run
(defmethod append-propspecs ((first propspec) (second propspec))
  (make-instance 'propspec
		 :props (append (slot-value first 'applications)
				(slot-value second 'applications))
		 :systems (loop with new = (slot-value first 'systems)
				for s in (slot-value second 'systems)
				do (pushnew s new)
				finally (return new))))

;; All knowledge of the possible combinator symbols should be confined to
;; between here and the end of the file -- i.e., if we are to add any
;; combinators, this is the code that needs to change

(defun compile-propapp (propapp)
  "Recursively apply the effects of property combinators in PROPAPP to produce
an atomic property application."
  (let ((sym (gensym)))
    (cond
      ;; UNAPPLY
      ((symbol-named unapply (car propapp))
       (destructuring-bind (psym . args) (compile-propapp (cadr propapp))
	 (setprop sym (proptype psym)
		  :desc (lambda (&rest args)
			  (strcat "Unapply: " (apply #'propdesc psym args)))
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
				 (apply #'propattrs psym args)
				 (mapc #'propappattrs propapps))
		    :check (get psym 'check)
		    :apply (lambda (&rest args)
			     (unless (eq :no-change
					 (apply psym args))
			       (loop for propapp in propapps
				     do (propappapply propapp))))
		    :unapply (lambda (&rest args)
			       (unless (eq :no-change
					   (apply #'propunapply psym args))
				 (loop for propapp in propapps
				       do (propappapply propapp)))))
	   (cons sym args))))
      ;; atomic property application
      (t
       propapp))))

(defvar *remote-lisp* nil
  "Whether this Lisp is one started up within a call to DEPLOY*.")

(defmethod eval-propspec ((propspec propspec))
  "Apply properties as specified by PROPSPEC."
  ;; TODO should have this check in the closures produced by DEFPROP too, so
  ;; that we will catch attempts to programmatically apply :LISP properties.
  ;; for the check here, could offer a restart to apply all the properties up
  ;; to but not including the first :LISP property (we don't just want to
  ;; apply all non-:LISP because that might violate dependencies established
  ;; by the order of the elements of PROPSPEC's props)
  (when (and (subtypep (class-of *connection*) 'posix-connection)
	     (eq :lisp (propspec->type propspec)))
    (error "Cannot apply :LISP properties using a POSIX connection"))
  ;; Don't try to load systems if we are a remote Lisp, as we don't upload the
  ;; .asd files, and we don't want to load out of /usr/share/common-lisp as we
  ;; might get a different version of the library at worst, or a lot of
  ;; warnings at best
  (unless *remote-lisp*
    (loop for system in (slot-value propspec 'systems)
	  unless (asdf:component-loaded-p system)
	    do (asdf:load-system system)))
  (loop for form in (slot-value propspec 'applications)
	for propapp = (compile-propapp form)
	do (let ((change-made (not (eq :no-change (propappapply propapp)))))
	     (format t "~@[~A :: ~]~@[~A ... ~]~:[ok~;done~]~%"
		     (get-hostname)
		     (propappdesc propapp)
		     change-made))))

(defmethod propspec->type ((propspec propspec))
  "Return :lisp if any types of the properties to be applied by PROPSPEC is
:lisp, else return :posix."
  (loop for form in (slot-value propspec 'applications)
	for propapp = (compile-propapp form)
	if (eq (propapptype propapp) :lisp)
	  return :lisp
	finally (return :posix)))

(defun props (forms &optional (systems nil systems-supplied-p))
  "Where FORMS is the elements of an unevaluated property application
specification, return code which will evaluate the expressions and produce the
corresponding property application specification.

SYSTEMS is the 'systems attribute of the property application specification
that the returned code should produce.

Intended for use by macros which allow the user to provide expressions instead
of values as the arguments to properties when building a property application
specification."
  (labels ((dedot-symbol (s)
	     (let ((n (symbol-name s)))
	       (intern (subseq n 0 (1- (length n))) (symbol-package s))))
	   (special-eval (args)
	     (let ((first (if (and (listp (car args))
				   (or (keywordp (caar args))
				       (and (listp (caar args))
					    (keywordp (caaar args)))))
			      `(quote ,(car args))
			      (car args)))
		   (rest (nreverse (cdr (reverse (cdr args))))))
	       `(,first ,@rest ,(props (lastcar args)))))
	   (make-eval-propspec (form)
	     (if (atom form)
		 `(quote ,form)
		 (destructuring-bind (first . rest) form
		   (if (and (symbolp first)
			    (not (member (symbol-name first)
					 '("UNAPPLY")
					 :test #'string=)))
		       (if (char= #\. (last-char (symbol-name first)))
			   `(list ',(dedot-symbol first)
				  ,@(special-eval rest))
			   `(list ',first ,@rest))
		       `(list ,@(mapcar #'make-eval-propspec form)))))))
    `(make-propspec
      ,@(and systems-supplied-p `(:systems ,systems))
      :props (list ,@(mapcar #'make-eval-propspec forms)))))
