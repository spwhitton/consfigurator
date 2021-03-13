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

(defun map-propspec-propapps (function propspec &optional env)
  "Map FUNCTION over each propapp occurring in PROPSPEC after macroexpansion.
FUNCTION designates a pure function from propapps to propapps.  PROPSPEC is a
property application specification expression."
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
  (let* ((occurrent
	   (delete-duplicates
	    (delete-if-not #'isprop (flatten (macroexpand-all propspec env)))))
	 (gensyms (loop for p in occurrent collect (gensym)))
	 ;; We need to substitute twice like this because if FUNCTION returns
	 ;; a form whose car is a member of OCCURRENT (as indeed it often will
	 ;; be), we will get stuck in an infinite macroexpansion.
	 (first-macrolets
	   (loop for p in occurrent and g in gensyms
		 collect `(,p (&rest args)
			      (cons ',g args))))
	 (second-macrolets
	   (loop for p in occurrent and g in gensyms
		 collect `(,g (&rest args)
			      (funcall ,(ensure-function function)
				       (cons ',p args))))))
    ;; At least SB-CLTL2:MACROEXPAND-ALL leaves the MACROLET in, so use CADDR
    ;; to remove it again -- if that turns out to be implementation-specific,
    ;; our MACROEXPAND-ALL should be responsible for looking for it and
    ;; stripping if necessary.  This is not just to avoid leaking our
    ;; implementation to our callers -- if the MACROLET is not stripped after
    ;; the first expansion, we'll be back in an infinite macroexpansion loop.
    (caddr (macroexpand-all
	    `(macrolet ,second-macrolets
	       ,(caddr (macroexpand-all
			`(macrolet ,first-macrolets ,propspec)
			env)))
	    env))))

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
to deploy this propspec.")
   (preprocessed-propspec
    :initarg :preprocessed-propspec
    :documentation
    "Preprocessed propspec corresponding to the propspec represented by this
PROPSPEC object.  A preprocessed propspec is not itself a valid propspec, so
the value of this slot should be considered opaque."))
  (:documentation
   "Object representing a propspec; specifically, a property application
specification expression associated with a list of ASDF systems.  Use
MAKE-PROPSPEC to create instances of this class.

The only valid methods operating directly on instances of this class are
PROPSPEC-SYSTEMS, APPEND-PROPSPECS, EVAL-PROPSPEC and PRINT-OBJECT."))

(defun make-propspec (&key (systems nil systems-supplied-p) propspec)
  "Convert a property application specification expression into a property
application specification proper by associating it with a list of ASDF
systems."
  (let ((preprocessed (map-propspec-propapps
		       (lambda (propapp)
			 (destructuring-bind (prop . args) propapp
			   `',(cons prop (apply (proppp prop) args))))
		       propspec)))
    (if systems-supplied-p
	(make-instance 'propspec :systems systems
				 :preprocessed-propspec preprocessed)
	(make-instance 'propspec :preprocessed-propspec preprocessed))))

;; since there are no unquoted propapps remaining in the propspec, we could
;; use MAKE-PROPSPEC here, but it is simpler just to use MAKE-INSTANCE
(defmethod print-object ((propspec propspec) stream)
  (format stream "#.~S" `(make-instance
			  'propspec
			  :systems ',(slot-value propspec 'systems)
			  :preprocessed-propspec
			  ',(slot-value propspec 'preprocessed-propspec)))
  propspec)

;; likewise, there aren't any unquoted propapps in either of FIRST and SECOND,
;; so we could use MAKE-PROPSPEC, but it's simpler and more efficient not to
(defmethod append-propspecs ((first propspec) (second propspec))
  (make-instance
   'propspec
   :systems (union (slot-value first 'systems)
		   (slot-value second 'systems))
   :preprocessed-propspec `(silent-seqprops
			    ,(slot-value first 'preprocessed-propspec)
			    ,(slot-value second 'preprocessed-propspec))))

(defvar *suppress-loading-systems* nil
  "Bound by code which needs to prevent EVAL-PROPSPEC from attempting to load
the ASDF systems associated with the propspec to be evaluated.")

(defmethod eval-propspec ((propspec propspec))
  (unless *suppress-loading-systems*
    (dolist (system (propspec-systems propspec))
      (unless (asdf:component-loaded-p system)
	(asdf:load-system system))))
  (eval (slot-value propspec 'preprocessed-propspec)))

(defmacro props (combinator &rest forms &aux replaced-propapps)
  "Apply variadic COMBINATOR to FORMS and convert from an unevaluated property
application specification expression to a property application specification."
  (labels ((replace-propapp (propapp)
	     (let ((gensym (gensym)))
	       (push (cons gensym propapp) replaced-propapps)
	       gensym))
	   (walk (tree)
	     (if (atom tree)
		 (if-let ((propapp (assoc tree replaced-propapps)))
		   `(list ',(cadr propapp) ,@(cddr propapp))
		   `',tree)
		 `(list ,@(mapcar #'walk tree)))))
    `(make-propspec
      :propspec ,(walk (map-propspec-propapps #'replace-propapp
					      (cons combinator forms))))))


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
