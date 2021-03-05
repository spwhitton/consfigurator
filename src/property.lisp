;;; Consfigurator -- Lisp declarative configuration management system

;;; Copyright (C) 2020-2021  Sean Whitton <spwhitton@spwhitton.name>

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

;;;; Properties

;; Properties are not stored as CLOS objects (or structs) in value cells
;; because they are immutable -- see "Attempting to work with anonymous
;; properties or connection types" in the docs.  A determined user could of
;; course edit the symbol plist entries and/or function cell, but we want to
;; make it a bit more difficult for someone who hasn't read that part of the
;; docs to accidentally violate immutability.

(defun setprop (sym type &key args desc preprocess hostattrs check apply unapply)
  ;; use non-keyword keys to avoid clashes with other packages
  (when type
    (setf (get sym 'type) type))
  (when args
    (setf (get sym 'args) args))
  (when desc
    (setf (get sym 'desc) desc))
  (when preprocess
    (setf (get sym 'preprocess) preprocess))
  (when hostattrs
    (setf (get sym 'hostattrs) hostattrs))
  (when check
    (setf (get sym 'check) check))
  (if apply
      (progn (setf (get sym 'apply) apply)
	     (setf (fdefinition sym)
		   (if check
		       (lambda (&rest args)
			 (unless (apply check args)
			   (apply apply args)))
		       apply)))
      (setf (fdefinition sym) #'noop))
  (when unapply
    (setf (get sym 'unapply) unapply))
  (setf (get sym 'property) t)
  sym)

(defun isprop (prop)
  (and (symbolp prop) (get prop 'property nil)))

(defun proptype (prop)
  (get prop 'type))

(defun proppp (prop)
  (get prop 'preprocess (lambda (&rest args) args)))

(defun propapptype (propapp)
  (get (car propapp) 'type))

(defun collapse-types (&rest lists)
  (if (member :posix (flatten lists)) :posix :lisp))

(defun propdesc (prop &rest args)
  (apply (get prop 'desc #'noop) args))

(defun propappdesc (propapp)
  (apply #'propdesc propapp))

(defun propargs (prop)
  (get prop 'args))

(defun propattrs (prop &rest args)
  (apply (get prop 'hostattrs #'noop) args))

(defun propappattrs (propapp)
  (apply #'propattrs propapp))

(defun propcheck (prop &rest args)
  (apply (get prop 'check #'noop) args))

(defun propappcheck (propapp)
  (apply #'propcheck propapp))

(defun propappapply (propapp)
  (apply (symbol-function (car propapp)) (cdr propapp)))

(defun propunapply (prop &rest args)
  (apply (get prop 'unapply #'noop) args))

(defun propappunapply (propapp)
  (apply #'propunapply propapp))

;;; supported way to write properties is to use one of these two macros

(defmacro defprop (name type args &body forms)
  (let ((slots (list :args (list 'quote args))))
    ;; if first element of forms is a plain string, consider it a docstring,
    ;; and ignore
    (when (stringp (car forms)) (pop forms))
    (loop for form in forms
	  if (keywordp (car form))
	  do (setf (getf slots (car form)) (cdr form)))
    (loop for kw in '(:desc :preprocess :hostattrs :check :apply :unapply)
	  do (if-let ((slot (getf slots kw)))
	       (setf (getf slots kw)
		     ;; inside this lambda we could do some checking of, e.g.,
		     ;; whether we are :lisp but this connection is
		     ;; posix-connection.  possibly a condition with a restart
		     ;; which allows skipping over this property
		     `(lambda ,args ,@slot))))
    `(setprop ',name ,type ,@slots)))

(defmacro defproplist (name type args &body properties)
  "Define a property which applies a property application specification.
ARGS is an ordinary lambda list, so you can use &AUX variables to compute
intermediate values.  PROPERTIES is an unevaluated property application
specification, but it will not be evaluated until the resulting property has
been added to a host, so it should not contain any free variables other than
as would be bound by (lambda ARGS).

The evaluation of PROPERTIES, and the evaluation of any &AUX variables, should
not have any side effects.  The evaluation will take place in the root Lisp.

If the first element of PROPERTIES is a string, it will be considered a
docstring for the resulting property.  If the first element of PROPERTIES
after any such string is a list beginning with :DESC, the remainder will be
used as the :DESC subroutine for the resulting property, like DEFPROP.

It is usually better to use this macro to combine several smaller properties
rather than writing a property which programmatically calls other properties.
This is because using this macro takes care of calling property :HOSTATTRS
subroutines at the right time."
  (when (stringp (car properties)) (pop properties))
  (let ((new-args (cons (gensym)
			(loop for arg in args
			      if (symbol-named &aux arg)
				return accum
			      else collect arg into accum)))
	;; TODO :UNAPPLY which unapplies in reverse order
	(slots (list :hostattrs '(lambda (propspec &rest ignore)
				   (declare (ignore ignore))
				   (%eval-propspec-hostattrs *host* propspec))
		     :apply '(lambda (propspec &rest ignore)
			       (declare (ignore ignore))
			       (eval-propspec propspec)))))
    (when (and (listp (car properties)) (eq :desc (caar properties)))
      (setf (getf slots :desc)
	    `(lambda ,new-args
	       (declare (ignorable ,@new-args))
	       ,@(cdr (pop properties)))))
    (setf (getf slots :preprocess)
	  `(lambda (&rest all-args)
	     (cons (destructuring-bind ,args all-args ,(props properties))
		   all-args)))
    `(setprop ',name ,type ,@slots)))


;;;; hostattrs in property subroutines

(define-condition inapplicable-property (error)
  ((text :initarg :text :reader inapplicable-property-text))
  (:report (lambda (condition stream)
	     (format stream "~A" (inapplicable-property-text condition)))))

(defun get-hostattrs (k)
  "Retrieve the list of static informational attributes of type KEY.

Called by property :HOSTATTRS, :APPLY and :UNAPPLY subroutines."
  (getf (slot-value *host* 'hostattrs) k))

(defun push-hostattrs (k &rest vs)
  "Push new static informational attributes VS of type KEY.

Called by property :HOSTATTRS subroutines."
  (dolist (v vs)
    (push v (getf (slot-value *host* 'hostattrs) k))))

(defun require-data (iden1 iden2)
  "Wrapper around PUSH-HOSTATTRS to indicate that a piece of prerequisite data
is needed to deploy a property.

Called by property :HOSTATTRS subroutines."
  (push-hostattrs :data (cons iden1 iden2)))

(defun get-hostname ()
  "Get the hostname of the host to which properties are being applied.

Called by property subroutines."
  (car (get-hostattrs :hostname)))
