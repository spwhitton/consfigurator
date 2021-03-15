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

(define-condition ambiguous-propspec (undefined-function) ())

(define-condition invalid-or-ambiguous-propspec (error)
  ((broken-propspec :initarg :propspec :reader broken-propspec))
  (:report
   (lambda (condition stream)
     (format
      stream
"MACROEXPAND-ALL could not process the following propspec.  This can happen
because the propspec is invalid, or because it contains references to
properties whose definitions have not been loaded.

Ensure that all functions, properties and property combinators used in a
propspec are defined before that propspec is processed by Consfigurator.

~A"
      (broken-propspec condition)))))

(defvar *replaced-propapps* nil
  "Internal dynamic variable used in MAP-PROPSPEC-PROPAPPS.")

(defun map-propspec-propapps
    (function propspec &optional reconstruct env &aux *replaced-propapps*)
  "Map FUNCTION over each propapp occurring in PROPSPEC after macroexpansion.
FUNCTION designates a pure function from propapps to propapps. PROPSPEC is a
property application specification expression.

RECONSTRUCT is a boolean flag indicating whether to return code which will
evaluate to the resultant propspec rather than that propspec itself; if t,
FUNCTION too should return code which will evaluate to propapps rather than
propapps themselves.  This is useful for when this function is called by
macros.  ENV is the ENV argument to be passed along to MACROEXPAND-ALL.

Note that this implementation will fail to map propapps appearing within the
arguments to properties in propapps, but that should not be needed."
  ;; The work of this function cannot be implemented fully portably.  See
  ;;
  ;;     Michael Raskin.  2017.  Writing a best-effort portable code walker in
  ;;     Common Lisp.  In Proceedings of 10th European Lisp Symposium, Vrije
  ;;     Universiteit Brussel, Belgium, April 2017 (ELS2017).
  ;;     DOI: 10.5281/zenodo.3254669
  ;;
  ;; for why.  However, it can be implemented in terms of MACROEXPAND-ALL,
  ;; whose semantics are conventionally well-understood and which is available
  ;; in most implementations of Common Lisp (we use the
  ;; trivial-macroexpand-all library to get at these implementations).
  (labels
      ((macrolet-and-expand (macrolets form)
	 (multiple-value-bind (expanded supported env-supported)
	     (trivial-macroexpand-all:macroexpand-all
	      `(macrolet ,macrolets ,form) env)
	   (unless supported
	     (error "Don't know how to MACROEXPAND-ALL in this Lisp."))
	   (when (and env (not env-supported))
	     (error "Don't know how to MACROEXPAND-ALL with env in this Lisp."))
	   ;; At least SB-CLTL2:MACROEXPAND-ALL leaves the MACROLET in, so use
	   ;; CADDR to remove it again -- if that turns out to be
	   ;; implementation-specific, we can look for what we added and
	   ;; remove it.
	   ;;
	   ;; This is not just to avoid leaking our implementation to our
	   ;; callers -- if we call this function more than once with old
	   ;; calls to MACROLET left in, we can get stuck in infinite macro
	   ;; expansion loops.
	   (caddr expanded)))
       (walk (tree)
	 (if (atom tree)
	     (if-let ((propapp (gethash tree *replaced-propapps*)))
	       (funcall function propapp)
	       (if reconstruct `',tree tree))
	     (let ((walked (mapcar #'walk tree)))
	       (if reconstruct (cons 'list walked) walked)))))
    ;; First we need to find all the propapps, after macro expansion.
    ;; Propapps contain the arguments to be passed to properties rather than
    ;; expressions which will evaluate to those arguments, and some of these
    ;; might be lists, which will look like invalid function calls to the code
    ;; walker.  So we macrolet every known property so that the code walker
    ;; does not assume these arguments are to be evaluated as arguments to
    ;; ordinary functions are.
    ;;
    ;; We can't just set up the macrolets to map FUNCTION over the propapp and
    ;; return the result because if FUNCTION returns a propapp whose car is
    ;; the same (as indeed it often will be) then we would get stuck in an
    ;; infinite macro expansion.  So we substitute back and forth for gensyms.
    (let ((expanded
	    (handler-case
		(macrolet-and-expand *known-property-macrolets* propspec)
	      (error ()
		(error 'invalid-or-ambiguous-propspec :propspec propspec)))))
      ;; Now we use a dummy macro expansion pass to find any symbols without
      ;; function definitions occurring in function call positions.  These
      ;; could potentially be properties whose definitions have not been
      ;; loaded -- especially since we get called at compile time by PROPS --
      ;; and if so, we would return an incorrect result because the previous
      ;; step will not have identified all the propapps in the propspec.  So
      ;; error out if we detect that situation.
      (macrolet-and-expand
       (loop for leaf in (delete-duplicates (flatten expanded))
	     if (and (symbolp leaf) (not (isprop leaf)))
	       collect `(,leaf (&rest args)
			       (unless (fboundp ',leaf)
				 (error 'ambiguous-propspec :name ',leaf))
			       ;; return something which looks like an
			       ;; ordinary function call to the code walker,
			       ;; so that it will recurse into ARGS
			       (cons (gensym) args)))
       expanded)
      ;; Finally, substitute the mapped propapps back in to the propspec.
      (let ((*replaced-propapps*
	      (alist-hash-table *replaced-propapps* :test 'eq)))
	(walk expanded)))))

(defmacro in-consfig (systems)
  "Sets the variable *CONSFIG* in the current package to SYSTEMS, or (SYSTEMS)
if SYSTEMS is an atom.  Used at the top of your consfig, right after IN-PACKAGE.

This is used to record a list of the names of the ASDF systems in which you
define your hosts, site-specific properties and deployments.  These systems
should depend on the \"consfigurator\" system.

SYSTEMS should satisfy the following condition: in normal usage of
Consfigurator, evaluating
(mapc #'asdf:load-system (if (atom SYSTEMS) (list SYSTEMS) SYSTEMS) should be
sufficient to define all the properties you intend to apply to hosts and
property combinators you intend to use in specifying propspecs.

Consfigurator uses this information when starting up remote Lisp images to
effect deployments: it sends over the ASDF systems specified by SYSTEMS."
  (setq systems (ensure-cons systems))
  (let ((sym (intern "*CONSFIG*")))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (defparameter ,sym ',systems
    "ASDF systems the loading of all of which is sufficient to define all the
Consfigurator properties and property combinators code in this symbol's
package applies to hosts."))))

(defclass propspec ()
  ((systems
    :initarg :systems
    :initform (or (symbol-value (find-symbol "*CONSFIG*"))
		  (error
		   "Looks like *CONSFIG* is not set; please call IN-CONSFIG"))
    :reader propspec-systems
    :documentation
    "List of names of ASDF systems, the loading of all of which is sufficient
to evaluate and to deploy this propspec."))
  (:documentation
   "Abstract superclass for propspecs.  Do not instantiate."))

(defclass preprocessed-propspec (propspec)
  ((preprocessed-propspec-expression
    :initarg :propspec
    :documentation
    "Preprocessed propspec corresponding to the propspec represented by this
object.  A preprocessed propspec is not itself a valid propspec, so the value
of this slot should be considered opaque."))
  (:documentation
   "A propspec which has been preprocessed.  The only valid methods operating
directly on instances of this class are PROPSPEC-SYSTEMS, EVAL-PROPSPEC and
PRINT-OBJECT."))

(defclass unpreprocessed-propspec (propspec)
  ((propspec-expression
    :initarg :propspec
    :reader propspec-props)))

(defgeneric preprocess-propspec (propspec)
  (:documentation
   "Quote all propapps in PROPSPEC, after calling :PREPROCESS subroutines."))

(defmethod preprocess-propspec ((propspec unpreprocessed-propspec))
  (make-instance 'preprocessed-propspec
		 :systems (propspec-systems propspec)
		 :propspec (map-propspec-propapps
			    (lambda (propapp)
			      (destructuring-bind (prop . args) propapp
				`',(cons prop (apply (proppp prop) args))))
			    (propspec-props propspec))))

(defun make-propspec (&key (systems nil systems-supplied-p) propspec)
  "Convert a property application specification expression into a property
application specification proper by associating it with a list of ASDF
systems."
  (if systems-supplied-p
      (make-instance 'unpreprocessed-propspec
		     :systems systems :propspec propspec)
      (make-instance 'unpreprocessed-propspec :propspec propspec)))

(defmethod print-object ((propspec preprocessed-propspec) stream)
  (format stream "#.~S" `(make-instance
			  'preprocessed-propspec
			  :systems ',(slot-value propspec 'systems)
			  :propspec
			  ',(slot-value propspec
					'preprocessed-propspec-expression)))
  propspec)

;; this could be defined for preprocessed propspecs easily enough but we
;; shouldn't need to append those
(defmethod append-propspecs
    ((first unpreprocessed-propspec) (second unpreprocessed-propspec))
  (make-propspec :systems (union (propspec-systems first)
				 (propspec-systems second))
		 :propspec `(silent-seqprops ,(propspec-props first)
					     ,(propspec-props second))))

(defvar *suppress-loading-systems* nil
  "Bound by code which needs to prevent EVAL-PROPSPEC from attempting to load
the ASDF systems associated with the propspec to be evaluated.")

(defmethod eval-propspec ((propspec preprocessed-propspec))
  (unless *suppress-loading-systems*
    (dolist (system (propspec-systems propspec))
      (unless (asdf:component-loaded-p system)
	(asdf:load-system system))))
  (eval (slot-value propspec 'preprocessed-propspec-expression)))

(define-condition ambiguous-unevaluated-propspec (ambiguous-propspec) ()
  (:report
   (lambda (condition stream)
     (format
      stream
      "The function, property or property combinator ~A is undefined.

Ensure that all functions, properties and property combinators used in an
unevaluated propspec are defined before that unevaluated propspec is
processed."
      (cell-error-name condition)))))

(defmacro props (combinator &rest forms)
  "Apply variadic COMBINATOR to FORMS and convert from an unevaluated property
application specification expression to a property application specification."
  (flet ((evaluate (propapp)
	   `(list ',(car propapp) ,@(cdr propapp))))
    (let ((propspec
	    (handler-case
		(map-propspec-propapps #'evaluate (cons combinator forms) t)
	      (ambiguous-propspec (c)
		;; resignal with a more specific error message
		(error 'ambiguous-unevaluated-propspec
		       :name (cell-error-name c))))))
      `(make-propspec :propspec ,propspec))))


;;;; Property combinators

(defmacro define-function-property-combinator (name args &body body)
  (multiple-value-bind (forms declarations docstring)
      (parse-body body :documentation t)
    `(defun ,name ,args
       ,@docstring
       ,@declarations
       (flet ((retprop (&rest all &key args &allow-other-keys)
		(let ((psym (gensym))
		      (setprop-args (remove-from-plist all :args)))
		  (apply #'setprop psym setprop-args)
		  (return-from ,name (list* psym args)))))
	 ,@forms))))

(define-function-property-combinator eseqprops (&rest propapps)
  (retprop :type (collapse-types (mapcar #'propapptype propapps))
	   :check (constantly nil)
	   :hostattrs (lambda () (mapc #'propappattrs propapps))
	   :apply (lambda () (apply-and-print propapps))))

(define-function-property-combinator seqprops (&rest propapps)
  (retprop :type (collapse-types (mapcar #'propapptype propapps))
	   :check (constantly nil)
	   :hostattrs (lambda () (mapc #'propappattrs propapps))
	   :apply (lambda ()
		    (handler-bind
			((failed-change
			   (lambda (c)
			     (declare (ignore c))
			     (invoke-restart 'skip-property))))
		      (apply-and-print propapps)))))

(define-function-property-combinator silent-seqprops (&rest propapps)
  (retprop :type (collapse-types (mapcar #'propapptype propapps))
	   :check (constantly nil)
	   :hostattrs (lambda () (mapc #'propappattrs propapps))
	   :apply (lambda ()
		    (handler-bind
			((failed-change
			   (lambda (c)
			     (declare (ignore c))
			     (invoke-restart 'skip-property))))
		      (mapc #'propappapply propapps)))))

;; note that the :FAILED-CHANGE value is only used within this function and
;; should not be returned by property subroutines, per the spec
(defun apply-and-print (propapps)
  (dolist (propapp propapps)
    (let* ((result (restart-case (propappapply propapp)
		     (skip-property () :failed-change)))
	   (status (case result
		     (:no-change     "ok")
		     (:failed-change "failed")
		     (t              "done"))))
      (format t "~@[~A :: ~]~@[~A ... ~]~A~%"
	      (get-hostname) (propappdesc propapp) status))))

(define-function-property-combinator unapply (propapp)
  (destructuring-bind (psym . args) propapp
    (retprop :type (proptype psym)
	     :lambda (propargs psym)
	     :desc (lambda (&rest args)
		     (strcat "Unapply: " (apply #'propdesc psym args)))
	     :check (complement (get psym 'check))
	     :apply (get psym 'unapply)
	     :unapply (get psym 'apply)
	     :args args)))
