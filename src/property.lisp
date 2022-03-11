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
    (setf (get sym 'punapply) unapply))
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

(defun propappargs (propapp)
  (if (and (listp (cadr propapp)) (member :orig-args (cadr propapp)))
      (getf (cadr propapp) :orig-args)
      (cdr propapp)))

(defun collapse-types (&rest lists)
  (if (member :lisp (flatten lists)) :lisp :posix))

(defun collapse-propapp-types (&rest lists)
  (if (member :lisp (mapcan (curry #'mapcar #'propapptype) lists))
      :lisp :posix))

(defun propdesc (prop &rest args)
  (apply (get prop 'desc (lambda-ignoring-args)) args))

(defun propappdesc (propapp)
  (when propapp
    (apply #'propdesc propapp)))

(defun proplambda (prop)
  (get prop 'plambda))

(defun propattrs (prop &rest args)
  (apply (get prop 'hostattrs (lambda-ignoring-args)) args))

(defun propappattrs (propapp)
  (when propapp
    (apply #'propattrs propapp)))

(defun propcheck (prop &rest args)
  (apply (get prop 'check (lambda-ignoring-args)) args))

(defun propappcheck (propapp)
  (if propapp (apply #'propcheck propapp) t))

(defmacro with-some-errors-are-failed-change (&body forms)
  `(handler-case (progn ,@forms)
     (run-failed (c)
       (failed-change "~&Unhandled failed command: ~%~A" c))))

(defun propapply (prop &rest args)
  (with-some-errors-are-failed-change
    (if (aand (get prop 'check) (apply it args))
        :no-change
        (apply (get prop 'papply (constantly :no-change)) args))))

(defun propappapply (propapp)
  (if propapp
      (apply #'propapply propapp)
      :no-change))

(defun propunapply (prop &rest args)
  (with-some-errors-are-failed-change
    (let ((check (get prop 'check))
          (apply (get prop 'papply))
          (unapply (get prop 'punapply)))
      ;; Only fail if there's no :UNAPPLY when there is an :APPLY, because
      ;; that is the case in which we can't do what was requested.  If there
      ;; is no :APPLY then we can infer that there is nothing on the host to
      ;; unapply (this will be the case for pure :HOSTATTRS properties).
      (cond
        ((or (and (not apply) (not unapply))
             (and check (not (apply check args))))
         :no-change)
        (unapply
         (apply unapply args))
        (apply
         (failed-change
"Attempt to unapply property with :APPLY subroutine but no :UNAPPLY subroutine."))))))

(defun propappunapply (propapp)
  (if propapp
      (apply #'propunapply propapp)
      :no-change))

(defvar *known-properties* nil
  "All properties whose definitions have been loaded.")

(defun record-known-property (psym)
  (unless (get psym 'isprop)
    (setf (get psym 'isprop) t)
    (push psym *known-properties*)))

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
dotted name alongside NAME.

With the current implementation, for properties whose lambda lists are such
that the dotted propapp rule regarding the last required or optional parameter
is applicable, optional parameters other than the last become required, and
information about whether or not optional parameters were supplied (supplied-p
parameters) is lost.  This is not much of a limitation in practice, however,
because in order to supply an embedded unevaluated propspec as the value of
the &rest parameter, any other optional parameters must be supplied too.  When
only the dotted propapp rule regarding the first parameter is applicable, that
argument becomes required, but the rest of the supplied parameters are passed
through unmodified, so supplied-p information is preserved."
  (multiple-value-bind (required optional rest kwargs)
      (parse-ordinary-lambda-list args :allow-specializers nil)
    (let* ((will-props (not (or rest kwargs)))
           (main (nconc required (mapcar #'car optional)))
           (firstsym (car main))
           (first (and firstsym
                       `(if (and (listp ,firstsym)
                                 (or (keywordp (car ,firstsym))
                                     (and (listp (car ,firstsym))
                                          (keywordp (caar ,firstsym)))))
                            `',,firstsym
                            ,firstsym)))
           (middle (butlast (if first (cdr main) main)))
           (new-args
             (if will-props
                 (setq rest (lastcar main)
                       main (nconc (nbutlast main) (list '&rest rest)))
                 (list* '&whole whole
                        ;; Strip default values (so we don't evaluate those
                        ;; forms here and also in the property), and strip
                        ;; supplied-p parameters for good measure as we will
                        ;; not use them.
                        (loop for elt in (ordinary-ll-without-&aux args)
                              if (listp elt)
                                collect (list (car elt) nil)
                              else collect elt)))))
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
                                         (ordinary-ll-without-&aux args)))))
                (list* ',name ,first (cddr ,whole))))
             (t
              `((declare (ignore ,@(ordinary-ll-variable-names
                                    (ordinary-ll-without-&aux args))))
                (cons ',name (cdr ,whole)))))))))

(defmacro define-property-defining-macro
    (mname (typev lambdav slotsv formsv &optional (namev (gensym))) &body mbody)
  "Define macro MNAME which be used to define properties, and which works by
parsing FORMSV and pushing SETPROP keyword argument pairs to plist SLOTSV."
  (multiple-value-bind (mforms mdeclarations mdocstring)
      (parse-body mbody :documentation t)
    (declare (ignore mdeclarations))
    (with-gensyms (body declarations)
      `(defmacro ,mname (,namev ,typev ,lambdav &body ,body)
         ,@(and mdocstring `(,mdocstring))
         (let ((programmatic-warning t)
               (,slotsv (list :type ,typev :lambda `',,lambdav)))
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
                    (record-known-property ',,namev))
                  (store-indentation-info-for-emacs ',,namev ',,lambdav ,indent)
                  (setprop ',,namev ,@,slotsv)
                  (define-dotted-property-macro ,,namev ,,lambdav)
                  ;; Now prepare a DEFUN for the property, to enable calling
                  ;; it programmatically within the :APPLY and :UNAPPLY
                  ;; routines of other properties.  This can lead to clearer
                  ;; code than going via DEFPROPSPEC/DEFPROPLIST for simple
                  ;; things like installing packages.
                  ,@(and
                     (getf ,slotsv :apply)
                     `((defun-with-args ,,namev args ,,lambdav
                         (unless *connection*
                           (simple-program-error
"Attempt to programmatically apply property with no connection established.

Common causes of this error are (i) missing the '.' to enable the dotted
propapp rules; and (ii) inadvertently calling the contents of a property's
function cell, instead of constructing a propapp, within DEFPROPSPEC."))
                         ;; Properties with :HOSTATTRS subroutines which set
                         ;; new hostattrs should not be used programmatically
                         ;; in this way, so issue a warning.
                         ;;
                         ;; TODO If this property is defined using
                         ;; DEFPROPLIST/DEFPROPSPEC and has no user-supplied
                         ;; :HOSTATTRS subroutine of its own, but one of the
                         ;; properties in the returned propspec does, then we
                         ;; won't issue the warning, but we should.
                         ,@(and programmatic-warning
                                (getf ,slotsv :hostattrs)
                                `((warn 'programmatic-apply-hostattrs
                                        :property ',,namev)))
                         (consfigure (cons ',,namev args)))))))))))))

(define-condition programmatic-apply-hostattrs (warning)
  ((property :initarg :property))
  (:report (lambda (condition stream)
             (format stream "Calling property ~S,
which has :HOSTATTRS subroutine, programmatically.  Use DEFPROPLIST/DEFPROPSPEC
to avoid trouble.  Use IGNORING-HOSTATTRS to muffle this warning if
~:*~S does not push any new hostattrs."
                     (slot-value condition 'property)))))

(defmacro ignoring-hostattrs (form)
  "Where FORM is a programmatic call to a property which has a :HOSTATTRS
subroutine, muffle warnings about calling a property with a :HOSTATTRS
subroutine programmatically.  Use this only when you know that the :HOSTATTRS
subroutine does not push any new hostattrs."
  (unless
      (and (listp form) (or (not (fboundp (car form)))
                            (isprop (car form))
                            (and (listp (cadr form))
                                 (eql 'function (caadr form))
                                 (isprop (cadadr form)))))
    (simple-program-error "~A is not a programmatic call to a property." form))
  `(handler-bind ((programmatic-apply-hostattrs #'muffle-warning))
     ,form))

;; supported ways to write properties are DEFPROP, DEFPROPSPEC and DEFPROPLIST

(define-property-defining-macro defprop (type lambda slots forms name)
  "Define a property by providing code for its subroutines."
  (loop for form in forms
        if (keywordp (car form))
          do (setf (getf slots (car form)) (cdr form)))
  (loop for kw in '(:desc :preprocess :hostattrs :check :apply :unapply)
        do (if-let ((slot (getf slots kw)))
             (multiple-value-bind (forms declarations) (parse-body slot)
               (setf (getf slots kw)
                     `(lambda ,lambda
                        ,@(and (member kw '(:desc :hostattrs))
                               `((declare
                                  (ignorable
                                   ,@(ordinary-ll-variable-names
                                      (ordinary-ll-without-&aux lambda)
                                      :include-supplied-p t)))))
                        ,@declarations
                        (block ,name ,@forms)))))))

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
the :DESC subroutine for the resulting property, like DEFPROP.  Supplying
:CHECK and :HOSTATTRS subroutines in the same way is also supported.
Otherwise, the body defines a function of the arguments specified by the
lambda list which returns the property application specification expression to
be evaluated and applied.  It should be a pure function aside from retrieving
hostattrs (as set by other properties applied to the hosts to which the
resulting property is applied, not as set by the properties in the returned
propspec).

Macro property combinators should be usable in the normal way in the body, but
some other macros commonly used in DEFHOST and DEFPROPLIST forms will not work
as expected.  In particular, the macros implementing dotted propapp notation
expect to be used within unevaluated property application specification
expressions and may not behave as expected in the body of DEFPROPSPEC.  You
can work around this particular limitation using the PROPAPP macro.  See
DISK:RAW-IMAGE-BUILT-FOR for an example of this technique.

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
          (let ((propapp (eval-propspec (getf plist :propspec))))
            (assert-connection-supports (propapptype propapp))
            (propappapply propapp))))
  (setf (getf slots :unapply)
        '(lambda (plist)
          (propappunapply (eval-propspec (getf plist :propspec)))))
  (loop while (and (listp (car forms)) (keywordp (caar forms)))
	do (setf (getf slots (caar forms))
		 `(lambda (plist)
                    (with-*host*-*consfig*
		      (destructuring-bind ,lambda (getf plist :orig-args)
                        ,@(and (member (caar forms) '(:desc :hostattrs))
                               `((declare
                                  (ignorable
                                   ,@(ordinary-ll-variable-names
                                      lambda :include-supplied-p t)))))
		        ,@(cdr (pop forms)))))))
  (unless (getf slots :hostattrs)
    (setq programmatic-warning nil))
  (setf (getf slots :hostattrs)
        `(lambda (plist)
           ,@(cddr (getf slots :hostattrs))
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
Supplying :CHECK and :HOSTATTRS subroutines in the same way is also supported.

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
  (alet (loop for remaining on properties
              for car = (car remaining)
              if (or (stringp car)
                     (and (listp car)
                          (member (car car)
                                  '(:desc :check :hostattrs declare))))
                collect car into begin
              else
                return (nreverse
                        (cons `(props eseqprops ,@remaining) begin)))
    `(defpropspec ,name ,type ,lambda ,@it)))


;;;; hostattrs in property subroutines

(define-simple-error inapplicable-property ()
  "Signal, in a :HOSTATTRS subroutine, that the host's hostattrs indicate that
this property cannot be applied to this host.  E.g. the property will try to
install an apt package but the host is FreeBSD.")

(defparameter *preprocessing-host* nil
  "HOST value currently being preprocessed.
Used by GET-HOSTATTRS to break infinite loops.")

(defun get-hostattrs
    (k &optional host &aux (host (ensure-host (or host *host*))))
  "Retrieve the list of static informational attributes of type KEY.

Called by property :HOSTATTRS, :APPLY and :UNAPPLY subroutines."
  ;; Ensure the host is preprocessed so the desired hostattrs are actually
  ;; there, assuming we're not already preprocessing it.  Avoid calling
  ;; PREPROCESS-HOST on PREPROCESSED-HOST values to avoid pointless copying.
  ;;
  ;; This is just to improve readability for some property definitions and
  ;; avoid confusing situations where hostattrs appear to be missing (for
  ;; example, if the hostname is not set until HOSTNAME:IS); properties which
  ;; will look up multiple hostattrs by supplying a value for HOST should call
  ;; PREPROCESS-HOST on that value themselves.
  (let ((host (if (and (subtypep (class-of host) 'unpreprocessed-host)
                       (not (eql host *preprocessing-host*)))
                  (preprocess-host host) host)))
    (getf (slot-value host 'hostattrs) k)))

(defun get-hostattrs-car (k &optional (host *host*))
  (car (get-hostattrs k host)))

(defun get-parent-hostattrs (k &optional (host *host*))
  (getf (get-hostattrs :parent-hostattrs host) k))

(defun get-parent-hostattrs-car (k &optional (host *host*))
  (car (get-parent-hostattrs k host)))

(defun push-hostattrs (k &rest vs)
  "Push new static informational attributes VS of type KEY.

Called by property :HOSTATTRS subroutines."
  (setf (getf (slot-value *host* 'hostattrs) k)
        (append vs (get-hostattrs k))))

(defun pushnew-hostattr (k v &key (test #'equal))
  "Push new static informational attribute V of type K.
TEST is passed on to PUSHNEW.  Called by property :HOSTATTRS subroutines."
  (pushnew-hostattrs k (list v) :test test))

(defun pushnew-hostattrs (k vs &key (test #'equal))
  "Push new static informational attributes VS of type K.
VS is a list of items.  TEST is passed on to PUSHNEW.  Called by property
:HOSTATTRS subroutines."
  (dolist (v (reverse vs))
    (pushnew v (getf (slot-value *host* 'hostattrs) k) :test test)))

(defun require-data (iden1 iden2)
  "Wrapper around PUSHNEW-HOSTATTR to indicate that a piece of prerequisite data
is needed to deploy a property.

Called by property :HOSTATTRS subroutines."
  (pushnew-hostattr :data (cons iden1 iden2)))

(defun get-hostname (&optional (host *host*))
  "Get the hostname of HOST, defaulting to the host to which properties are
being applied.

Called by property subroutines."
  (get-hostattrs-car :hostname host))

(defun get-short-hostname (&optional (host *host*))
  "Get the short hostname of HOST, defaulting to the host to which properties
are being applied.

Called by property subroutines."
  (car (split-string (get-hostattrs-car :hostname host) :separator ".")))


;;;; :APPLY subroutines

(define-simple-error failed-change ()
  "Signal problems with the connection and errors while actually attempting to
apply or unapply properties.")

(define-simple-error aborted-change (failed-change)
  "Like FAILED-CHANGE, except the attempt to apply or unapply the property has
failed before any changes have been made to the system.  Signalled when a
property is able to determine that it cannot be applied/unapplied by examining
the actual state of the host but without making any changes.

Not to be confused with INAPPLICABLE-PROPERTY.")

(defun maybe-writefile-string (path content &key (mode nil mode-supplied-p))
  "Wrapper around WRITEFILE which returns :NO-CHANGE and avoids writing PATH if
PATH already has the specified CONTENT and MODE."
  (if (and (remote-exists-p path)
           (multiple-value-bind (existing-mode existing-size)
               (remote-file-stats path)
             (and (or (not mode-supplied-p) (= mode existing-mode))
                  (and (>= (* 4 (length content)) existing-size)
                       (string= (readfile path) content)))))
      :no-change
      (apply #'writefile
             path content (and mode-supplied-p `(:mode ,mode)))))

(defun assert-euid-root ()
  "Assert that the remote user has uid 0 (root)"
  (unless (zerop (get-connattr :remote-uid))
    (failed-change "Property requires root to apply")))

(defun assert-connection-supports (type)
  (unless (or (eq type :posix) (lisp-connection-p))
    (failed-change
     "Cannot apply :LISP properties using a POSIX-type connection")))

(defun cksum (file)
  (parse-integer (car (split-string (run "cksum" file)))))

(defun local-cksum (file)
  (parse-integer
   (car
    (split-string
     (run-program `("cksum" ,(unix-namestring file)) :output :string)))))

;; this is a safe parse of ls(1) output given its POSIX specification
(defun ls-cksum (file)
  (when-let* ((ls (ignore-errors
                   (words (run :env '(:LC_ALL "C") "ls" "-dlL" file))))
              (ls-car (car ls))
              (ls-end (subseq ls 2 8)))
    (if (char= #\d (elt ls-car 0))
        (cons ls-car ls-end)
        (let ((cksum (ignore-errors (cksum file))))
          (and cksum (list* ls-car cksum ls-end))))))

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
       (if (or (eql result :no-change)
               (and ,before (equal ,before (ls-cksum ,file))))
           :no-change result))))

(defmacro with-change-if-changes-files ((&rest files) &body forms)
  "Execute FORMS and yield :NO-CHANGE if none of FILES change.
See WITH-CHANGE-IF-CHANGES-FILE docstring regarding the sense of 'change'."
  (with-gensyms (filesg beforeg)
    `(let* ((,filesg (list ,@files))
            (,beforeg (mapcar #'ls-cksum ,filesg))
            (result (progn ,@forms)))
       (if (or (eql result :no-change)
               (loop for file in ,filesg and before in ,beforeg
                     always before
                     always (equal before (ls-cksum file))))
           :no-change result))))

(defmacro with-change-if-changes-file-content ((file) &body forms)
  "Execute FORMS and yield :NO-CHANGE if FILE has the same content afterwards."
  (with-gensyms (before)
    `(let* ((,before (ignore-errors (cksum ,file)))
            (result (progn ,@forms)))
       (if (or (eql result :no-change)
               (and ,before (eql ,before (cksum ,file))))
           :no-change result))))

(defmacro with-change-if-changes-file-content-or-mode ((file) &body forms)
  "Execute FORMS and yield :NO-CHANGE if FILE has the same content and mode
afterwards."
  (with-gensyms (before)
    `(let* ((,before (ls-cksum ,file))
            (result (progn ,@forms)))
       (if (equal result :no-change)
           :no-change
           (let ((after (ls-cksum ,file)))
             (if (and ,before
                      (string= (car ,before) (car after) :start1 1 :start2 1)
                      (eql (cadr ,before) (cadr after)))
                 :no-change result))))))
