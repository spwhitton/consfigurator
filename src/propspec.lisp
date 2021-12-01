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
(named-readtables:in-readtable :consfigurator)

;;;; Property application specifications

(define-condition ambiguous-propspec (undefined-function) ()
  (:report
   (lambda (condition stream)
     (format
      stream
      "The function, property or property combinator ~A is undefined.

Ensure that all functions, properties and property combinators used in a
propspec are defined before that propspec is processed by Consfigurator."
      (cell-error-name condition)))))

(define-condition invalid-propspec (error)
  ((original-error  :initarg :error    :reader original-error)
   (broken-propspec :initarg :propspec :reader broken-propspec))
  (:report
   (lambda (condition stream)
     (format
      stream
      "The code walker could not process the following propspec.~%~%~S"
      (broken-propspec condition))
     (when (slot-boundp condition 'original-error)
       (format stream "~&~%The error from the code walker was:~%~%~A"
               (original-error condition))))))

(defun map-propspec-propapps (function propspec &optional reconstruct env)
  "Map FUNCTION over each propapp occurring in PROPSPEC after macroexpansion.
FUNCTION designates a pure function from propapps to propapps. PROPSPEC is a
property application specification expression.

RECONSTRUCT is a boolean flag indicating whether to return code which will
evaluate to the resultant propspec rather than that propspec itself; if t,
FUNCTION too should return code which will evaluate to propapps rather than
propapps themselves.  This is useful for when this function is called by
macros.  ENV is passed along to AGNOSTIC-LIZARD:WALK-FORM.

This implementation will fail to map propapps appearing within the arguments
to properties in propapps, but that should not be needed.  It can very
occasionally give incorrect results due to limitations of the Common Lisp
standard with respect to code walking; see \"Pitfalls\" in the Consfigurator
manual."
  (let* (replaced-propapps
         ;; First we need to find all the propapps, after macro expansion.
         ;; Propapps contain the arguments to be passed to properties rather
         ;; than expressions which will evaluate to those arguments, and some
         ;; of these might be lists, which will look like invalid function
         ;; calls to the code walker.  So we replace every known property so
         ;; that the code walker does not assume these arguments are to be
         ;; evaluated as arguments to ordinary functions are.
         (expanded
           (handler-case
               (agnostic-lizard:walk-form
                propspec env
                :on-macroexpanded-form
                (lambda (form env &aux (c (and (listp form) (car form))))
                  (declare (ignore env))
                  (cond ((and c (isprop c))
                         (aprog1 (gensym)
                           (push (cons it form) replaced-propapps)))
                        ;; We also look for any symbols without function or
                        ;; property definitions occurring in function call
                        ;; positions.  These could potentially be properties
                        ;; whose definitions have not been loaded --
                        ;; especially since we get called at compile time by
                        ;; PROPS -- and if so, we would return an incorrect
                        ;; result because the previous branch will not have
                        ;; identified all the propapps in the propspec.  So
                        ;; error out if we detect that situation.
                        ((and c (not (fboundp c)))
                         (error 'ambiguous-propspec :name c))
                        (t
                         form))))
             (ambiguous-propspec (c) (error c))
             (error (condition)
               (error 'invalid-propspec :error condition :propspec propspec))))
         (replaced-propapps
           (alist-hash-table replaced-propapps :test 'eq)))
    ;; Finally, substitute the mapped propapps back in to the propspec.
    (labels ((walk (tree)
               (if (atom tree)
                   (if-let ((propapp (gethash tree replaced-propapps)))
                     (funcall function propapp)
                     (if (and reconstruct (symbolp tree)) `',tree tree))
                   (let ((walked (mapcar #'walk tree)))
                     (if reconstruct (cons 'list walked) walked)))))
      (walk expanded))))

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

(define-condition no-consfig (simple-warning) ())

(defclass propspec ()
  ((systems
    :initarg :systems
    :initform
    (or (handler-case (symbol-value (find-symbol "*CONSFIG*"))
          (unbound-variable ()))
        (warn 'no-consfig :format-arguments `(,*package*) :format-control
"Initialising propspec without any list of ASDF systems supplied,
and *PACKAGE* is not a package for which IN-CONSFIG has been called.
Consfigurator may not be able to start up remote Lisp images to effect
deployments involving this propspec; see the docstring for IN-CONSFIG.

Either call IN-CONSFIG for ~S, explicitly pass
:SYSTEMS NIL to MAKE-PROPSPEC, or muffle this warning if code using this
propspec will not need to start up any remote Lisp images."))
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

(define-print-object-for-structlike preprocessed-propspec)

(define-print-object-for-structlike unpreprocessed-propspec)

;; this could be defined for preprocessed propspecs easily enough but we
;; shouldn't need to append those
(defmethod append-propspecs
    ((first unpreprocessed-propspec) (second unpreprocessed-propspec))
  (make-propspec
   :systems (union (propspec-systems first) (propspec-systems second))
   :propspec
   (let ((firstp (propspec-props first))
         (secondp (propspec-props second)))
     (if (and firstp secondp)
         (destructuring-bind (1first . 1rest) firstp
           (destructuring-bind (2first . 2rest) secondp
             ;; We used to unconditionally combine with SILENT-SEQPROPS but
             ;; (i) if either FIRSTP or SECONDP don't call APPLY-AND-PRINT
             ;; then properties get applied without any output being printed
             ;; which would normally be printed; and (ii) it implicitly
             ;; suppresses errors but we should only do that when SEQPROPS or
             ;; similar is used explicitly (or by DEFHOST).
             (cond ((and (eql 1first 2first)
                         (member 1first '(eseqprops seqprops)))
                    (cons 1first (append 1rest 2rest)))
                   ;; Already combined with sequencing combinators, so let
                   ;; them handle it.
                   ((and (member 1first '(eseqprops seqprops))
                         (member 2first '(eseqprops seqprops)))
                    `(silent-seqprops ,firstp ,secondp))
                   ;; Avoid a pointless nested ESEQPROPS.
                   ((eql 1first 'eseqprops)
                    `(eseqprops ,@1rest ,secondp))
                   ((eql 2first 'eseqprops)
                    `(eseqprops ,firstp ,@2rest))
                   ;; Default.
                   (t `(eseqprops ,firstp ,secondp)))))
         (or firstp secondp)))))

(defmethod append-propspecs ((first null) (second unpreprocessed-propspec))
  second)

(defmethod append-propspecs ((first unpreprocessed-propspec) (second null))
  first)

(defmethod append-propspecs ((first null) (second null))
  nil)

(defmethod eval-propspec ((propspec preprocessed-propspec))
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

(defmacro propapp (form)
  "Convert a single element of an unevaluated property application specification
expression to a property application specification expression."
  (flet ((evaluate (propapp)
           `(list ',(car propapp) ,@(cdr propapp))))
    (handler-case (map-propspec-propapps #'evaluate form t)
      (ambiguous-propspec (c)
        ;; resignal with a more specific error message
        (error 'ambiguous-unevaluated-propspec :name (cell-error-name c))))))

(defmacro props (combinator &rest forms)
  "Apply variadic COMBINATOR to FORMS and convert from an unevaluated property
application specification expression to a property application specification
expression."
  `(propapp ,(cons combinator forms)))
