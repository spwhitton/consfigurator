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
(named-readtables:in-readtable :consfigurator)

;;;; Properties

;; Properties are not stored as CLOS objects (or structs) in value cells
;; because they are immutable -- see "Attempting to work with anonymous
;; properties or connection types" in the docs.  A determined user could of
;; course edit the symbol plist entries and/or function cell, but we want to
;; make it a bit more difficult for someone who hasn't read that part of the
;; docs to accidentally violate immutability.

;; default TYPE to :LISP as we want an explicit declaration that something is
;; compatible with :POSIX-type connections
(defun setprop (sym &key (type :lisp) lambda desc preprocess hostattrs check apply unapply)
  ;; use non-keyword keys to avoid clashes with other packages
  (when type
    (setf (get sym 'ptype) type))
  (when lambda
    (setf (get sym 'plambda) lambda))
  (when desc
    (setf (get sym 'desc) desc))
  (when preprocess
    (setf (get sym 'preprocess) preprocess))
  (when hostattrs
    (setf (get sym 'hostattrs) hostattrs))
  (when check
    (setf (get sym 'check) check))
  (when apply
    (setf (get sym 'papply) apply))
  (when unapply
    (setf (get sym 'unapply) unapply))
  sym)

(defun isprop (prop)
  (and (symbolp prop) (get prop 'isprop nil)))

(defun proptype (prop)
  (get prop 'ptype))

(defun proppp (prop)
  (get prop 'preprocess (lambda (&rest args) args)))

(defun propapptype (propapp)
  (if propapp
      (get (car propapp) 'ptype)
      :posix))

(defun collapse-types (&rest lists)
  (if (member :posix (flatten lists)) :posix :lisp))

(defun propdesc (prop &rest args)
  (apply (get prop 'desc #'noop) args))

(defun propappdesc (propapp)
  (when propapp
    (apply #'propdesc propapp)))

(defun proplambda (prop)
  (get prop 'plambda))

(defun propattrs (prop &rest args)
  (apply (get prop 'hostattrs #'noop) args))

(defun propappattrs (propapp)
  (when propapp
    (apply #'propattrs propapp)))

(defun propcheck (prop &rest args)
  (apply (get prop 'check #'noop) args))

(defun propappcheck (propapp)
  (if propapp (apply #'propcheck propapp) t))

(defmacro with-some-errors-are-failed-change (&body forms)
  `(handler-case (progn ,@forms)
     (run-failed (c)
       (failed-change "~&Unhandled failed command: ~%~A" c))))

(defun propapply (prop &rest args)
  (with-some-errors-are-failed-change
    (let ((check (get prop 'check)))
      (if (and check (apply check args))
          :no-change
          (apply (get prop 'papply (constantly :no-change)) args)))))

(defun propappapply (propapp)
  (if propapp
      (apply #'propapply propapp)
      :no-change))

(defun propunapply (prop &rest args)
  (with-some-errors-are-failed-change
    (let ((check (get prop 'check)))
      (if (and check (not (apply check args)))
          :no-change
          (apply (get prop 'unapply (constantly :no-change)) args)))))

(defun propappunapply (propapp)
  (if propapp
      (apply #'propappunapply propapp)
      :no-change))

(defvar *known-properties* nil
  "All properties whose definitions have been loaded.")

(defvar *known-property-macrolets* nil
  "Macro definitions for all known properties as used in MAP-PROPSPEC-PROPAPPS.

This variable exists just to avoid consing these forms over and over again;
see MAP-PROPSPEC-PROPAPPS for how they are used.")

(defun record-known-property (psym)
  (unless (get psym 'isprop)
    (setf (get psym 'isprop) t)
    (push psym *known-properties*)
    (push `(,psym (&rest args)
                  (let ((gensym (gensym)))
                    (push (list* gensym ',psym args)
                          *replaced-propapps*)
                    gensym))
          *known-property-macrolets*)))

(defun dump-properties-for-emacs (from to)
  (let ((put-forms
          (stripln
           (with-output-to-string (s)
             (loop
               for (prop . indent)
                 in (nreverse (mappend (lambda (s) (get s 'indent))
                                       *known-properties*))
               do (format s "  (put '~(~A~) 'common-lisp-indent-function '~A)~%"
                          prop indent))))))
    (with-open-file (in from)
      (with-open-file (out to :direction :output :if-exists :supersede)
        (loop for line = (read-line in nil)
              while line
              do (princ (re:regex-replace "  @putforms@" line put-forms) out)
                 (terpri out))))))

(defun store-indentation-info-for-emacs (sym args &optional info)
  (unless (string-prefix-p "%" (symbol-name sym))
    (let* ((package-short-name
             (lastcar (split-string (package-name *package*) :separator ".")))
           (short-name
             (if (string= package-short-name "CONSFIGURATOR")
                 (symbol-name sym)
                 (strcat package-short-name ":" (symbol-name sym))))
           (dotted-name (strcat short-name "."))
           (dotted-exported
             (eql :external (nth-value
                             1
                             (find-symbol (strcat (symbol-name sym) ".")
                                          (symbol-package sym)))))
           indent)
      (cond
        (info
	 (push (cons short-name info) indent)
         (when dotted-exported
	   (push (cons dotted-name info) indent)))
        ((and dotted-exported (not (find '&key args)))
	 (let ((n (1- (loop with n = 0
                            for arg in args
                            if (member arg '(&rest &body))
                              return (1+ n)
                            if (eql arg '&aux)
                              return n
                            unless (eq arg '&optional)
                              do (incf n)
                            finally (return n)))))
           (when (plusp n)
             (push (cons dotted-name n) indent)))))
      (when indent
        (setf (get sym 'indent) indent)))))

(defmacro with-*host*-*consfig* (&body forms)
  `(progv `(,(intern "*CONSFIG*"))
       `(,(propspec-systems (host-propspec *host*)))
     ,@forms))

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
              `(`(,',name ,,first ,,@middle (make-propspec
                                             :propspec (props eseqprops ,@,rest)))))
             (will-props
              `(`(,',name ,,@middle (make-propspec
                                     :propspec (props eseqprops ,@,rest)))))
             (first
              `((declare (ignore ,@(cdr (ordinary-ll-variable-names
                                         (ordinary-ll-without-&aux args)
                                         :include-supplied-p t))))
                (list* ',name ,first (cddr ,whole))))
             (t
              `((declare (ignore ,@(ordinary-ll-variable-names
                                    (ordinary-ll-without-&aux args)
                                    :include-supplied-p t)))
                (cons ',name (cdr ,whole)))))))))

(defmacro define-property-defining-macro
    (mname (typev lambdav slotsv formsv) &body mbody)
  "Define macro MNAME which be used to define properties, and which works by
parsing FORMSV and pushing SETPROP keyword argument pairs to plist SLOTSV."
  (multiple-value-bind (mforms mdeclarations mdocstring)
      (parse-body mbody :documentation t)
    (declare (ignore mdeclarations))
    (with-gensyms (name body declarations)
      `(defmacro ,mname (,name ,typev ,lambdav &body ,body)
         ,@(and mdocstring `(,mdocstring))
         (let ((,slotsv (list :type ,typev :lambda `',,lambdav)))
           (multiple-value-bind (,formsv ,declarations)
               (parse-body ,body :documentation t)
             (when (> (length ,declarations) 1)
               (error "Multiple DECLARE forms unsupported."))
             ,@mforms
             (let ((indent (cadr (find-if (lambda (e)
					    (form-beginning-with indent e))
					  (cdar ,declarations)))))
               `(progn
                  (eval-when (:compile-toplevel :load-toplevel :execute)
                    (record-known-property ',,name))
                  (store-indentation-info-for-emacs ',,name ',,lambdav ,indent)
                  (setprop ',,name ,@,slotsv)
                  (define-dotted-property-macro ,,name ,,lambdav)
                  ;; Now prepare a DEFUN for the property, to enable calling
                  ;; it programmatically within the :APPLY and :UNAPPLY
                  ;; routines of other properties.  This can lead to clearer
                  ;; code than going via DEFPROPSPEC/DEFPROPLIST for simple
                  ;; things like installing packages.
                  ,@(and
                     (getf ,slotsv :apply)
                     `((defun-with-args ,,name args ,,lambdav
                         ;; Properties with :HOSTATTRS subroutines which set
                         ;; new hostattrs should not be used programmatically
                         ;; in this way, so issue a warning.
                         ,@(and (getf ,slotsv :hostattrs)
                                '((programmatic-apply-hostattrs)))
                         (%consfigure
                          nil
                          (make-host
			   :hostattrs (hostattrs *host*)
                           :propspec
                           (with-*host*-*consfig*
                             (make-propspec
                              :propspec (cons ',,name args))))))))))))))))

(define-condition programmatic-apply-hostattrs (simple-warning) ())

(defun programmatic-apply-hostattrs ()
  (warn 'programmatic-apply-hostattrs
        :format-control
        "Calling property which has :HOSTATTRS subroutine programmatically.
Use DEFPROPLIST/DEFPROPSPEC to avoid trouble."))

(defmacro ignoring-hostattrs (form)
  "Where FORM is a programmatic call to a property which has a :HOSTATTRS
subroutine, muffle warnings about calling a property with a :HOSTATTRS
subroutine programmatically.  Use this only when you know that the :HOSTATTRS
subroutine does not push any new hostattrs."
  (unless (and (listp form) (isprop (car form)))
    (simple-program-error "~A is not a programmatic call to a property." form))
  `(handler-bind ((programmatic-apply-hostattrs #'muffle-warning))
     ,form))

;; supported ways to write properties are DEFPROP, DEFPROPSPEC and DEFPROPLIST

(define-property-defining-macro defprop (type lambda slots forms)
  "Define a property by providing code for its subroutines."
  (loop for form in forms
        if (keywordp (car form))
          do (setf (getf slots (car form)) (cdr form)))
  (loop for kw in '(:desc :preprocess :hostattrs :check :apply :unapply)
        do (if-let ((slot (getf slots kw)))
             (setf (getf slots kw)
                   ;; TODO wrap a BLOCK around ,@slot with the property name,
                   ;; so we can return from it
                   `(lambda ,lambda
                      ,@(and (member kw '(:desc :hostattrs))
                             `((declare
                                (ignorable
                                 ,@(ordinary-ll-variable-names
                                    (ordinary-ll-without-&aux lambda))))))
                      ,@slot)))))

(define-property-defining-macro defpropspec (type lambda slots forms)
  "Define a property which constructs, evaluates and applies a propspec.

This is how you can define a property which works by calling other properties,
in accordance with property combinators.

Except in very simple cases, it is usually better to use this macro (or
DEFPROPLIST) to combine several smaller properties rather than writing a
property using DEFPROP which programmatically calls other properties. This is
because using this macro takes care of calling property :HOSTATTRS
subroutines at the right time.

If the first element of the body is a string, it will be considered a
docstring for the resulting property.  If the first element of the body after
any such string is a list beginning with :DESC, the remainder will be used as
the :DESC subroutine for the resulting property, like DEFPROP.  Supplying a
:CHECK subroutine in the same way is also supported.  Otherwise, the body
defines a function of the arguments specified by the lambda list which returns
the property application specification expression to be evaluated and applied.
It should be a pure function aside from retrieving hostattrs (as set by other
properties applied to the hosts to which the resulting property is applied,
not as set by the properties in the returned propspec).

You can usually use DEFPROPLIST instead of DEFPROPSPEC, which see."
  ;; This is implemented by effectively pushing a null pointer to the front of
  ;; the propapp's arguments at :PREPROCESS-time, calling the user's code with
  ;; the other arguments to the propapp at :HOSTATTRS-time, and storing the
  ;; resulting propspec at the other end of the pointer, so that the :APPLY
  ;; and :UNAPPLY subroutines can get at it.  We have to keep the original
  ;; arguments to the propapp around for the sake of the :DESC and :CHECK
  ;; subroutines.
  (setf (getf slots :preprocess)
        '(lambda (&rest args)
          (list (list :propspec nil :orig-args args))))
  (setf (getf slots :apply)
        '(lambda (plist)
          (propappapply (eval-propspec (getf plist :propspec)))))
  (setf (getf slots :unapply)
        '(lambda (plist)
          (propappunapply (eval-propspec (getf plist :propspec)))))
  (loop while (and (listp (car forms)) (keywordp (caar forms)))
	do (setf (getf slots (caar forms))
		 `(lambda (plist)
		    (destructuring-bind ,(ordinary-ll-without-&aux lambda)
			(getf plist :orig-args)
                      ,@(and (member (caar forms) '(:desc :hostattrs))
                             `((declare
                                (ignorable
                                 ,@(ordinary-ll-variable-names
                                    (ordinary-ll-without-&aux lambda))))))
		      ,@(cdr (pop forms))))))
  (setf (getf slots :hostattrs)
        `(lambda (plist)
           (let ((propspec (with-*host*-*consfig*
                               (preprocess-propspec
                                (make-propspec
                                 :propspec (destructuring-bind ,lambda
                                               (getf plist :orig-args)
                                             ,@forms))))))
             (setf (getf plist :propspec) propspec)
             (propappattrs (eval-propspec propspec))))))

(defmacro defproplist (name type lambda &body properties)
  "Like DEFPROPSPEC, but define the function which yields the propspec using the
unevaluated property application specification PROPERTIES, where the implicit
surrounding combinator is ESEQPROPS.

If the first element of PROPERTIES is a string, it will be considered a
docstring for the resulting property.  If the first element of PROPERTIES
after any such string is a list beginning with :DESC, the remainder will be
used as the :DESC subroutine for the resulting property, like DEFPROP.
Supplying a :CHECK subroutine in the same way is also supported.

Otherwise, the body should not contain any references to variables other than
those in LAMBDA.  LAMBDA is an ordinary lambda list, so you can use &AUX
variables to compute intermediate values.  The evaluation of arguments to
propapps in PROPERTIES, and the evaluation of any &AUX variables in LAMBDA,
will happen at :HOSTATTRS-time for the host to which the resulting property is
to be applied, so you can retrieve static informational attributes set by
other properties applied to the host (unlike with unevaluated property
application specifications appearing in DEFHOST forms).  The evaluation should
otherwise be purely functional.

You will usually be able to use DEFPROPLIST instead of DEFPROPSPEC.  However,
sometimes you will need to fall back on DEFPROPSPEC.  For example, an
unevaluated property application specification cannot express passing values
other than constant values and propapps to property combinators."
  (let ((propspec
          (loop for remaining on properties
                for car = (car remaining)
                if (or (stringp car)
                       (and (listp car) (member (car car)
						'(:desc :check declare))))
                  collect car into begin
                else
                  return (nreverse
                          (cons `(props eseqprops ,@remaining) begin)))))
    `(defpropspec ,name ,type ,lambda ,@propspec)))


;;;; hostattrs in property subroutines

(define-simple-error inapplicable-property
  "Signal, in a :HOSTATTRS subroutine, that the host's hostattrs indicate that
this property cannot be applied to this host.  E.g. the property will try to
install an apt package but the host is FreeBSD.")

(defun get-hostattrs (k)
  "Retrieve the list of static informational attributes of type KEY.

Called by property :HOSTATTRS, :APPLY and :UNAPPLY subroutines."
  (getf (slot-value *host* 'hostattrs) k))

(defun get-hostattrs-car (k)
  (car (get-hostattrs k)))

(defun get-parent-hostattrs (k)
  (getf (get-hostattrs :parent-hostattrs) k))

(defun get-parent-hostattrs-car (k)
  (car (get-parent-hostattrs k)))

(defun push-hostattrs (k &rest vs)
  "Push new static informational attributes VS of type KEY.

Called by property :HOSTATTRS subroutines."
  (setf (getf (slot-value *host* 'hostattrs) k)
        (append vs (get-hostattrs k))))

(defun pushnew-hostattrs (k &rest vs)
  "Push new static informational attributes VS of type KEY.

Called by property :HOSTATTRS subroutines."
  (dolist (v (reverse vs))
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

(define-simple-error failed-change
  "Signal problems with the connection and errors while actually attempting to
apply or unapply properties.")

(defun call-with-os (f &rest args)
  (apply (ensure-function f) (get-hostattrs-car :os) args))

(defun assert-euid-root ()
  "Assert that the remote user has uid 0 (root)"
  (if-let ((uid (slot-value *connection* 'remote-uid)))
    (unless (zerop uid)
      (failed-change "Property requires root to apply"))
    (multiple-value-bind (out err exit)
        (run :may-fail "id" "-u")
      (unless (zerop exit)
        (failed-change #?"Failed to run id(1) on remote system: ${err}"))
      (let ((new-uid (parse-integer out)))
        (unless (zerop new-uid)
          (failed-change "Property requires root to apply"))
        (setf (slot-value *connection* 'remote-uid) new-uid)))))

(defun get-user ()
  "Get the remote username."
  (or (slot-value *connection* 'remote-user)
      (setf (slot-value *connection* 'remote-user)
            (multiple-value-bind (match groups)
                (re:scan-to-strings "^uid=[0-9]+\\(([^)]+)" (mrun "id"))
              (and match (elt groups 0))))))

(defun assert-connection-supports (type)
  (unless (or (eq type :posix) (lisp-connection-p))
    (failed-change
     "Cannot apply :LISP properties using a POSIX-type connection")))

(defun cksum (file)
  (ignore-errors (parse-integer (car (split-string (mrun "cksum" file))))))

;; this is a safe parse of ls(1) output given its POSIX specification
(defun ls-cksum (file)
  (let ((ls (ignore-errors
             (split-string (mrun :env '(:LOCALE "C") "ls" "-dlL" file))))
        (cksum (cksum file)))
    (when (and ls cksum)
      (list* (car ls) cksum (subseq ls 2 8)))))

(defmacro with-change-if-changes-file ((file) &body forms)
  "Execute FORMS and yield :NO-CHANGE if FILE does not change.
Since stat(1) is not POSIX, this is implemented by calling `ls -dlL' and
cksum(1), and seeing if any of the information reported there, except for the
number of links, has changed.  Thus, you should not use this macro to detect
changes in properties which will change the file but not the output of `ls
-dlL' and cksum(1)."
  (with-gensyms (before)
    `(let* ((,before (ls-cksum ,file))
            (result (progn ,@forms)))
       (if (and ,before (equal ,before (ls-cksum ,file)))
           :no-change result))))

(defmacro with-change-if-changes-file-content ((file) &body forms)
  "Execute FORMS and yield :NO-CHANGE if FILE has the same content afterwards."
  (with-gensyms (before)
    `(let* ((,before (cksum ,file))
            (result (progn ,@forms)))
       (if (and ,before (eql ,before (cksum ,file)))
           :no-change result))))

(defmacro with-change-if-changes-file-content-or-mode ((file) &body forms)
  "Execute FORMS and yield :NO-CHANGE if FILE has the same content and mode
afterwards."
  (with-gensyms (before)
    `(let* ((,before (ls-cksum ,file))
            (result (progn ,@forms)))
       (let ((after (ls-cksum ,file)))
         (if (and ,before
                  (string= (car ,before) (car after) :start1 1 :start2 1)
                  (eql (cadr ,before) (cadr after)))
             :no-change result)))))
