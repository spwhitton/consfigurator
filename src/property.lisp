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
(named-readtables:in-readtable :interpol-syntax)

;;;; Properties

;; Properties are not stored as CLOS objects (or structs) in value cells
;; because they are immutable -- see "Attempting to work with anonymous
;; properties or connection types" in the docs.  A determined user could of
;; course edit the symbol plist entries and/or function cell, but we want to
;; make it a bit more difficult for someone who hasn't read that part of the
;; docs to accidentally violate immutability.

;; default TYPE to :LISP as we want an explicit declaration that something is
;; compatible with :POSIX-type connections
(defun setprop (sym &key (type :lisp) lambda desc preprocess hostattrs check apply unapply indent)
  ;; use non-keyword keys to avoid clashes with other packages
  (when type
    (setf (get sym 'type) type))
  (when lambda
    (setf (get sym 'lambda) lambda))
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
			 (if (apply check args)
			     :no-change
			     (apply apply args)))
		       apply)))
      (setf (fdefinition sym) (lambda (&rest ignore)
				(declare (ignore ignore))
				:no-change)))
  (when unapply
    (setf (get sym 'unapply) unapply))
  (store-indentation-info-for-emacs sym lambda indent)
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

(defvar *known-properties* nil
  "All properties whose definitions have been loaded.")

(defvar *known-property-macrolets* nil
  "Macro definitions for all known properties as used in MAP-PROPSPEC-PROPAPPS.

This variable exists just to avoid consing these forms over and over again;
see MAP-PROPSPEC-PROPAPPS for how they are used.")

(defun record-known-property (psym)
  (push psym *known-properties*)
  (push `(,psym (&rest args)
		(let ((gensym (gensym)))
		  (push (list* gensym ',psym args)
			*replaced-propapps*)
		  gensym))
	*known-property-macrolets*))

(defun dump-properties-for-emacs (from to)
  (let ((put-forms
	  (stripln
	   (with-output-to-string (s)
	     (loop
	       for (prop . indent)
		 in (nreverse (mappend (lambda (s) (get s 'indent))
				       *known-properties*))
	       do (format s "  (put '~A 'common-lisp-indent-function '~A)~%"
			  prop indent))))))
    (with-open-file (in from)
      (with-open-file (out to :direction :output :if-exists :supersede)
	(loop for line = (read-line in nil)
	      while line
	      do (princ (re:regex-replace "  @putforms@" line put-forms) out)
		 (terpri out))))))

(defun store-indentation-info-for-emacs (sym args &optional info)
  (let* ((package-short-name
	   (lastcar (split-string (package-name *package*) :separator ".")))
	 (short-name
	   (string-downcase
	    (if (string= package-short-name "CONSFIGURATOR")
		(symbol-name sym)
		(strcat package-short-name ":" (symbol-name sym)))))
	 (dotted-name (strcat short-name "."))
	 indent)
    (cond
      (info
       (push (cons short-name info) indent)
       (push (cons dotted-name info) indent))
      ((not (find '&key args))
       (let ((n (1- (loop with n = 0
			  for arg in args
			  if (member arg '(&rest &body &aux))
			    return (1+ n)
			  unless (eq arg '&optional)
			    do (incf n)
			  finally (return n)))))
	 (when (plusp n)
	   (push (cons dotted-name n) indent)))))
    (when indent
      (setf (get sym 'indent) indent))))

(defmacro define-dotted-property-macro (name args &aux (whole (gensym)))
  "Affix a period to the end of NAME and define a macro expanding into a
propapp calling the original NAME after applying the dotted propapp rules,
to the extent that doing so makes sense given the structure of ARGS.

For most properties this is a dummy definition which will not be exported.
However, for properties where someone might like to use the dotted propapp
rules in unevaluated propspecs containing calls to the property, export the
dotted name alongside NAME."
  (multiple-value-bind (required optional rest kwargs)
      (parse-ordinary-lambda-list args :allow-specializers nil)
    (let* ((will-props (not (or rest kwargs)))
	   (main (nconc required optional))
	   (firstsym (ensure-car (car main)))
	   (first (and firstsym
		       `(if (and (listp ,firstsym)
				 (or (keywordp (car ,firstsym))
				     (and (listp (car ,firstsym))
					  (keywordp (caar ,firstsym)))))
			    `',,firstsym
			    ,firstsym)))
	   (middle (mapcar #'ensure-car (butlast (if first (cdr main) main))))
	   (new-args
	     (if will-props
		 (setq rest (ensure-car (lastcar main))
		       main (nconc (nbutlast main) (list '&rest rest)))
		 (nconc (list '&whole whole) (ordinary-ll-without-&aux args)))))
      `(defmacro ,(format-symbol (symbol-package name) "~A." name) ,new-args
	 ,@(cond
	     ((and first will-props)
	      `(`(,',name ,,first ,,@middle (props eseqprops ,@,rest))))
	     (will-props
	      `(`(,',name ,,@middle (props eseqprops ,@,rest))))
	     (first
	      `((declare (ignore ,@(cdr (ordinary-ll-variable-names
					 (ordinary-ll-without-&aux args)))))
		(list* ',name ,first (cddr ,whole))))
	     (t
	      `((declare (ignore ,@(ordinary-ll-variable-names
				    (ordinary-ll-without-&aux args))))
		(cons ',name (cdr ,whole)))))))))

;;; supported way to write properties is to use one of these two macros

(defmacro defprop (name type args &body body)
  (let ((slots (list :type type :lambda (list 'quote args))))
    (multiple-value-bind (forms declarations)
	(parse-body body :documentation t)
      (when (> (length declarations) 1)
	(error "Multiple DECLARE forms unsupported."))
      (when-let ((indent (cadr (assoc 'indent (cdar declarations)))))
	(setf (getf slots :indent) indent))
      (loop for form in forms
	    if (keywordp (car form))
	      do (setf (getf slots (car form)) (cdr form)))
      (loop for kw in '(:desc :preprocess :hostattrs :check :apply :unapply)
	    do (if-let ((slot (getf slots kw)))
		 (setf (getf slots kw)
		       `(lambda ,args
			  ,@(and (eq type :lisp)
				 (member kw '(:check :apply :unapply))
				 `((assert-connection-supports :lisp)))
			  ,@slot))))
      `(eval-when (:compile-toplevel :load-toplevel :execute)
	 (record-known-property ',name)
	 (setprop ',name ,@slots)
	 (define-dotted-property-macro ,name ,args)))))

(defmacro defproplist (name type args &body properties)
  "Define a property which applies a property application specification.
ARGS is an ordinary lambda list, so you can use &AUX variables to compute
intermediate values.  PROPERTIES is an unevaluated property application
specification where the implicit surrounding combinator is ESEQPROP, but it
will not be converted to a propspec until the resulting property has been
added to a host, so it should not contain any free variables other than as
would be bound by (lambda ARGS).

The evaluation of PROPERTIES, and the evaluation of any &AUX variables, should
not have any side effects.  The evaluation will take place in the root Lisp.
In particular, at present, storing or retrieving static informational
attributes is not supported.

If the first element of PROPERTIES is a string, it will be considered a
docstring for the resulting property.  If the first element of PROPERTIES
after any such string is a list beginning with :DESC, the remainder will be
used as the :DESC subroutine for the resulting property, like DEFPROP.

It is usually better to use this macro to combine several smaller properties
rather than writing a property which programmatically calls other properties.
This is because using this macro takes care of calling property :HOSTATTRS
subroutines at the right time."
  (when (stringp (car properties)) (pop properties))
  (let ((new-args (cons (gensym) (ordinary-ll-without-&aux args)))
	;; TODO :UNAPPLY which unapplies in reverse order
	(slots (list :type type
		     :lambda `',args
		     :hostattrs '(lambda (propspec &rest ignore)
				  (declare (ignore ignore))
				  (%eval-propspec-hostattrs *host* propspec))
		     :apply '(lambda (propspec &rest ignore)
			      (declare (ignore ignore))
			      (eval-propspec propspec)))))
    (when (and (listp (car properties))
	       (eql 'declare (caar properties)))
      ;; currently INDENT is the only supported declaration so we can just
      ;; take the cadadr
      (setf (getf slots :indent) (cadadr (pop properties))))
    (when (and (listp (car properties)) (eq :desc (caar properties)))
      (setf (getf slots :desc)
	    `(lambda ,new-args
	       (declare (ignorable ,@new-args))
	       ,@(cdr (pop properties)))))
    (setf (getf slots :preprocess)
	  `(lambda (&rest all-args)
	     (cons (destructuring-bind ,args all-args
		     (props eseqprops ,@properties))
		   all-args)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (record-known-property ',name)
       (setprop ',name ,@slots)
       (define-dotted-property-macro ,name ,args))))


;;;; hostattrs in property subroutines

(define-condition inapplicable-property (error)
  ((text :initarg :text :reader inapplicable-property-text))
  (:report (lambda (condition stream)
	     (format stream "~A" (inapplicable-property-text condition)))))

(defun get-hostattrs (k)
  "Retrieve the list of static informational attributes of type KEY.

Called by property :HOSTATTRS, :APPLY and :UNAPPLY subroutines."
  (getf (slot-value *host* 'hostattrs) k))

(defun get-hostattrs-car (k)
  (car (get-hostattrs k)))

(defun push-hostattrs (k &rest vs)
  "Push new static informational attributes VS of type KEY.

Called by property :HOSTATTRS subroutines."
  (dolist (v vs)
    (push v (getf (slot-value *host* 'hostattrs) k))))

(defun pushnew-hostattrs (k &rest vs)
  "Push new static informational attributes VS of type KEY.

Called by property :HOSTATTRS subroutines."
  (dolist (v vs)
    (pushnew v (getf (slot-value *host* 'hostattrs) k))))

(defun require-data (iden1 iden2)
  "Wrapper around PUSH-HOSTATTRS to indicate that a piece of prerequisite data
is needed to deploy a property.

Called by property :HOSTATTRS subroutines."
  (pushnew-hostattrs :data (cons iden1 iden2)))

(defun get-hostname ()
  "Get the hostname of the host to which properties are being applied.

Called by property subroutines."
  (get-hostattrs-car :hostname))


;;;; :APPLY subroutines

;; INAPPLICABLE-PROPERTY is for :HOSTATTRS subroutines, FAILED-CHANGE is for
;; problems with the connection and errors while actually attempting to apply

(define-condition failed-change (error)
  ((text :initarg :text :reader failed-change-text))
  (:report (lambda (condition stream)
	     (format stream "~A" (failed-change-text condition)))))

(defun call-with-os (f &rest args)
  (apply (ensure-function f) (get-hostattrs-car :os) args))

(defun assert-euid-root ()
  "Assert that the remote user has uid 0 (root)"
  (if-let (uid (slot-value *connection* 'remote-uid))
    (unless (zerop uid)
      (error 'failed-change :text "Property requires root to apply"))
    (multiple-value-bind (out err exit)
        (run :may-fail "id" "-u")
      (unless (zerop exit)
        (error 'failed-change
	       :text #?"Failed to run id(1) on remote system: ${err}"))
      (let ((new-uid (parse-integer out)))
        (unless (zerop new-uid)
          (error 'failed-change :text "Property requires root to apply"))
        (setf (slot-value *connection* 'remote-uid) new-uid)))))

(defun assert-connection-supports (type)
  (unless (or (eq type :posix) (lisp-connection-p))
    (error 'failed-change
	   "Cannot apply :LISP properties using a POSIX-type connection")))
