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

;;;; Property combinators

(defmacro define-function-property-combinator (name args &body body)
  "Define a function property combinator NAME with lambda list ARGS.

Usage notes:

- If you need to read individual arguments to propapps passed as arguments to
  NAME, call PROPAPP-ARGS to access them.  For passing a whole list of args on
  to a property subroutine, just take the cdr of the propapp.

  For an example showing both techniques at work, see POSTFIX:MAPPED-FILE."
  (multiple-value-bind (forms declarations docstring)
      (parse-body body :documentation t)
    `(defun ,name ,args
       ,@(and docstring `(,docstring))
       ,@declarations
       (flet ((:retprop (&rest all &key args &allow-other-keys)
                (let ((psym (gensym ,(symbol-name name)))
                      (setprop-args (remove-from-plist all :args)))
                  (unless (getf setprop-args :desc)
                    (setf (get ',name 'inline-combinator) t))
                  (setf (get psym 'combinator) ',name)
                  (apply #'setprop psym setprop-args)
                  (return-from ,name (list* psym args)))))
         ,@forms))))

(defmacro define-choosing-property-combinator
    (name lambda-list &key type choose)
  `(progn
     (define-function-property-combinator ,name ,lambda-list
       (flet ((choose-propapp () ,choose))
         (:retprop :type ,type
                   :desc (lambda (&rest args)
                           (declare (ignore args))
                           (propapp-desc (choose-propapp)))
                   :hostattrs (lambda (&rest args)
                                (declare (ignore args))
                                (propapp-attrs (choose-propapp)))
                   :apply (lambda (&rest args)
                            (declare (ignore args))
                            (apply-propapp (choose-propapp)))
                   :unapply (lambda (&rest args)
                              (declare (ignore args))
                              (unapply-propapp (choose-propapp))))))
     (setf (get ',name 'inline-combinator) t)))

;; There can be multiple SKIP-* restarts with the same name established at
;; once, and we need this handler to invoke one of the four established by the
;; call to APPLY-AND-PRINT right after we establish this handler.
(defmacro with-skip-failed-changes
    ((&key (condition ''failed-change) (restart ''skip-property)) &body forms)
  (once-only (condition restart)
    (with-gensyms (old-restarts)
      `(let* ((,old-restarts
                (loop for restart
                        in (compute-restarts (make-condition ,condition))
                      when (eql (restart-name restart) ,restart)
                        collect restart)))
         (handler-bind
             ((condition
                (lambda (c)
                  (when (subtypep (type-of c) ,condition)
                    (when (subtypep (type-of c) 'simple-condition)
                      (with-indented-inform
                        (apply #'informat t
                               (simple-condition-format-control c)
                               (simple-condition-format-arguments c))))
                    ;; We can't just use NSET-DIFFERENCE and take the
                    ;; LASTCAR because NSET-DIFFERENCE provides no ordering
                    ;; guarantees.
                    (loop with chosen and old-restarts = ,old-restarts
                          for restart in (compute-restarts c)
                          if (eql restart (car old-restarts))
                            do (pop old-restarts)
                          else if (eql (restart-name restart) ,restart)
                                 do (setq chosen restart)
                          finally (invoke-restart chosen))))))
           ,@forms)))))

(define-function-property-combinator eseqprops (&rest propapps)
  (:retprop :type (combine-propapp-types propapps)
            :hostattrs (lambda () (mapc #'propapp-attrs propapps))
            :apply (lambda () (apply-and-print propapps))
            :unapply (lambda () (apply-and-print propapps t))))

(define-function-property-combinator eseqprops-until (condition &rest propapps)
  "Like ESEQPROPS, but if CONDITION is signalled, handle it simply by skipping
remaining elements of PROPAPPS.  CONDITION usually names a subclass of
FAILED-CHANGE."
  (:retprop :type (combine-propapp-types propapps)
            :hostattrs (lambda () (mapc #'propapp-attrs propapps))
            :apply (lambda ()
                     (with-skip-failed-changes (:condition condition
                                                :restart 'skip-sequence)
                       (apply-and-print propapps)))
            :unapply (lambda ()
                       (with-skip-failed-changes (:condition condition
                                                  :restart 'skip-sequence)
                         (apply-and-print propapps t)))))

(define-function-property-combinator seqprops (&rest propapps)
  (:retprop :type (combine-propapp-types propapps)
            :hostattrs (lambda () (mapc #'propapp-attrs propapps))
            :apply (lambda ()
                     (with-skip-failed-changes ()
                       (apply-and-print propapps)))
            :unapply (lambda ()
                       (with-skip-failed-changes ()
                         (apply-and-print propapps t)))))

(defmacro with-requirements (propapp &body requirements)
  "Apply PROPAPP only after applying each dependency in REQUIREMENTS.
Each item in REQUIREMENTS implicitly depends on the one preceding it, i.e., we
apply the elements of REQUIREMENTS in reverse order."
  `(eseqprops ,@(reverse requirements) ,propapp))

(define-function-property-combinator silent-seqprops (&rest propapps)
  (:retprop :type (combine-propapp-types propapps)
            :hostattrs (lambda () (mapc #'propapp-attrs propapps))
            :apply (lambda ()
                     (with-skip-failed-changes ()
                       (apply-and-print propapps nil t)))
            :unapply (lambda ()
                       (with-skip-failed-changes ()
                         (apply-and-print propapps t t)))))

(defun apply-and-print
    (propapps &optional unapply silent
     &aux
       (buffer (make-array
                '(0) :element-type 'character :fill-pointer 0 :adjustable t))
       ;; Remove any null propapps because we don't want to print anything for
       ;; those, and applying them will do nothing.
       (propapps (remove nil (if unapply (reverse propapps) propapps))))
  (prog-changes
    (dolist (propapp propapps)
      (let* ((combinator (get (car propapp) 'combinator))
             (announce
               (and (not silent)
                    (or (> *consfigurator-debug-level* 2)
                        (not (get combinator 'inline-combinator)))
                    ;; We don't announce properties whose names begin with '%'
                    ;; and which have no description; these are typically
                    ;; DEFPROPs which exist only for use within a
                    ;; DEFPROPLIST/DEFPROPSPEC defining an exported property.
                    (not (and (< *consfigurator-debug-level* 3)
                              (char= #\% (char (symbol-name (car propapp)) 0))
                              (not (get (car propapp) 'desc)))))))
        (flet ((post-apply (status)
                 (when propapp
                   (when (and (plusp (length buffer))
                              (or silent
                                  (> *consfigurator-debug-level* 1)
                                  (not (string= status "ok"))))
                     (fresh-line)
                     (princ buffer))
                   (when announce
                     (informat t "~&~@[~A :: ~]~@[~A ... ~]~A~%"
                               (get-hostname) (propapp-desc propapp) status))
                   ;; Ensure POST-APPLY called exactly once for each propapp.
                   (setq propapp nil)))

               (test (c) (subtypep (type-of c) 'aborted-change))
               (ntest (c) (not (subtypep (type-of c) 'aborted-change)))

               (pareport (s)
                 (format s "Skip (~{~S~^ ~})"
                         (cons (car propapp) (propapp-args propapp))))
               (seqreport (s)
                 (format s "Skip remainder of sequence containing (~{~S~^ ~})"
                         (cons (car propapp) (propapp-args propapp)))))
          ;; In both the restarts and the fallback cleanup form, we treat any
          ;; non-local exit at all as though it were caused by the signalling
          ;; of a condition subtyping FAILED-CHANGE.  One possible improvement
          ;; might be to pass "failed" to POST-APPLY, and signal
          ;; SKIPPED-PROPERTIES, only when the non-local exit is due to a
          ;; condition subtyping ERROR.  Alternatively, maybe we could drop
          ;; FAILED-CHANGE and make ABORTED-CHANGE a direct subclass of ERROR.
          (unwind-protect
               ;; Establish restarts to be invoked by WITH-SKIP-FAILED-CHANGES
               ;; or possibly interactively by the user.  There are two of
               ;; each because we want to handle ABORTED-CHANGE specially.
               (restart-case
                   (alet (add-change
                          (if announce
                              (with-output-to-string (*standard-output* buffer)
                                (with-indented-inform
                                  (if unapply
                                      (unapply-propapp propapp)
                                      (apply-propapp propapp))))
                              (if unapply
                                  (unapply-propapp propapp)
                                  (apply-propapp propapp))))
                     (post-apply (if (eql it :no-change) "ok" "done")))
                 ;; Standard restarts for skipping over sequence entries.
                 (skip-property () :test ntest :report pareport
                   (signal 'skipped-properties) (post-apply "failed")
                   (add-change))
                 (skip-property () :test test :report pareport
                   (signal 'skipped-properties) (post-apply "failed"))
                 ;; Special restarts for the whole sequence which return from
                 ;; the enclosing DOLIST based on the kind of error.  If
                 ;; ABORTED-CHANGE, we assume that applying the current
                 ;; propapp made no change, so we return a value indicating
                 ;; whether properties earlier in PROPAPPS made a change.
                 ;; Otherwise, we assume that some change was made.
                 (skip-sequence () :test ntest :report seqreport
                   (signal 'skipped-properties) (post-apply "failed")
                   (return-from prog-changes))
                 (skip-sequence () :test test :report seqreport
                   (signal 'skipped-properties) (post-apply "failed")
                   (return-changes)))
            ;; Ensure we print out the buffer contents if due to a non-local
            ;; exit neither of the other calls to POST-APPLY have been made.
            (post-apply "failed")))
        (setf (fill-pointer buffer) 0)))))

(defmacro unapply (form)
  "Where FORM is a programmatic application of a property (i.e. an application
of a property directly inside an :APPLY or :UNAPPLY subroutine), unapply the
property instead of applying it."
  (destructuring-bind (property . args) form
    `(consfigure `(unapplied (,',property ,,@args)))))

(define-function-property-combinator unapplied (propapp)
  (destructuring-bind (psym . args) propapp
    (:retprop :type (proptype psym)
              :lambda (proplambda psym)
              :desc (lambda (&rest args)
                      (strcat "Unapplied: " (apply #'propdesc psym args)))
              :check (when-let ((check (get psym 'check)))
                       (complement check))
              :hostattrs (lambda (&rest args)
                           ;; run the :HOSTATTRS subroutine but throw away any
                           ;; new hostattrs; when unapplying, the :HOSTATTRS
                           ;; subroutine is only to check compatibility
                           (with-preserve-hostattrs
                             (apply #'propattrs psym args)))
              :apply (get psym 'punapply)
              :unapply (get psym 'papply)
              :args args)))

(define-function-property-combinator desc (desc propapp)
  (:retprop :type (propapp-type propapp)
            :desc (lambda () desc)
            :hostattrs (lambda (&rest args)
                         (declare (ignore args))
                         (propapp-attrs propapp))
            :apply (lambda (&rest args)
                     (declare (ignore args))
                     (apply-propapp propapp))
            :unapply (lambda (&rest args)
                       (declare (ignore args))
                       (unapply-propapp propapp))))

(defmacro on-change (propapp &body on-change)
  "If applying or unapplying PROPAPP makes a change, also apply each of the
propapps ON-CHANGE in order."
  `(on-change*
    ,propapp
    ,(if (cdr on-change) `(eseqprops ,@on-change) (car on-change))
    t))

(defmacro on-apply-change (propapp &body on-change)
  "If applying PROPAPP makes a change, also apply each of the propapps ON-CHANGE
in order."
  `(on-change*
    ,propapp
    ,(if (cdr on-change) `(eseqprops ,@on-change) (car on-change))))

(define-function-property-combinator on-change*
    (propapp on-change &optional unapply)
  (let ((prop (car propapp)))
    (:retprop :type (combine-propapp-types (list propapp on-change))
              :desc (get prop 'desc)
              :hostattrs (lambda (&rest args)
                           (apply #'propattrs prop args)
                           (propapp-attrs on-change))
              :apply (lambda (&rest args)
                       (aprog1 (apply #'propapply prop args)
                         (unless (eql it :no-change)
                           (apply-propapp on-change))))
              :unapply (lambda (&rest args)
                         (aprog1 (apply #'propunapply prop args)
                           (when (and unapply (not (eql it :no-change)))
                             (apply-propapp on-change))))
              :args (cdr propapp))))

(defmacro as (user &body properties)
  "Apply PROPERTIES as USER by reconnecting with the :AS connection type.
Note that the :AS connection type requires root, so as a special case, this
macro just expands to ESEQPROPS if USER is the literal string \"root\"
(without evaluation).  This makes it possible to use this macro to annotate
applications of properties which are normally applied by non-root, to make it
explicit that in this case they're being applied as root, e.g. that they will
affect /root and not /home."
  (if (and (stringp user) (string= user "root"))
      `(eseqprops ,@properties)
      `(reconnects. `((:as :user ,,user)) ,@properties)))

(defmacro with-flagfile (flagfile &body propapps)
  "Apply PROPAPPS unless FLAGFILE exists on the remote; after applying, create
FLAGFILE.
Useful to ensure that something is done just once.  Has the semantics that if
FLAGFILE exists, PROPAPPS are assumed to all be already applied."
  `(with-flagfile*
    ,flagfile
    ,(if (cdr propapps) `(eseqprops ,@propapps) (car propapps))))

(define-function-property-combinator with-flagfile* (flagfile propapp)
  (:retprop :type (propapp-type propapp)
            :desc (get (car propapp) 'desc)
            :hostattrs (get (car propapp) 'hostattrs)
            :check (lambda-ignoring-args
                     (remote-exists-p flagfile))
            :apply (lambda-ignoring-args
                     (prog1 (apply-propapp propapp)
                       (mrun "mkdir" "-p"
                             (pathname-directory-pathname flagfile))
                       (mrun "touch" flagfile)))
            :unapply (lambda-ignoring-args
                       (prog1 (unapply-propapp propapp)
                         (mrun "rm" "-f" flagfile)))
            :args (cdr propapp)))

(define-function-property-combinator with-unapply (&rest propapps)
  "As ESEQPROPS, except that if :UNAPPLY appears in PROPAPPS, then return a
property which applies the elements of PROPAPPS prior to :UNAPPLY, but which
when unapplied ignores the elements of PROPAPPS prior to :UNAPPLY, and instead
applies the elements of PROPAPPS appearing after :UNAPPLY.

Analogously to how DEFPROPLIST/DEFPROPSPEC allow you to define a property
which works by calling other properties, this combinator allows you to define
an :UNAPPLY subroutine for a property which works by calling other properties."
  (let* ((unapply (member :unapply propapps))
         (apply (ldiff propapps unapply))
         (apply-propapp
           (if (cdr apply) (apply #'eseqprops apply) (car apply)))
         (unapply-propapp (if (cddr unapply)
                              (apply #'eseqprops (cdr unapply))
                              (cadr unapply))))
    (if unapply
        (:retprop :type (combine-propapp-types apply (cdr unapply))
                  :hostattrs (lambda-ignoring-args
                               (propapp-attrs apply-propapp)
                               ;; as in definition of UNAPPLIED combinator
                               (with-preserve-hostattrs
                                 (propapp-attrs unapply-propapp)))
                  :apply (lambda-ignoring-args (apply-propapp apply-propapp))
                  :unapply (lambda-ignoring-args
                             (apply-propapp unapply-propapp)))
        apply-propapp)))

(defmacro with-homedir ((&key user dir) &body propapps)
  "Apply PROPAPPS with a different home and initial working directory, either
DIR or the home directory of USER."
  (when (and user dir)
    (simple-program-error
     "WITH-HOMEDIR: Both USER and DIR arguments supplied."))
  `(with-homedir* ,user ,dir
     ,(if (cdr propapps) `(eseqprops ,@propapps) (car propapps))))

(define-function-property-combinator with-homedir* (user dir propapp)
  (flet ((change (f)
           ;; Ensure the :CONSFIGURATOR-CACHE connattr is populated because
           ;; determining it may look at HOME.  In particular, we want to
           ;; avoid looking in the new HOME for cached data to upload.
           (when (lisp-connection-p) (get-connattr :consfigurator-cache))
           (let ((new (or dir (stripln (run (strcat "echo ~" user))))))
             (with-connattrs (:remote-home new)
               (with-remote-current-directory (new)
                 (if (lisp-connection-p)
                     (let ((orig (getenv "HOME")))
                       (setf (getenv "HOME") new)
                       (unwind-protect (funcall f propapp)
                         (setf (getenv "HOME") orig)))
                     (funcall f propapp)))))))
    (:retprop :type (propapp-type propapp)
              :desc (get (car propapp) 'desc)
              :hostattrs (get (car propapp) 'hostattrs)
              :apply (lambda-ignoring-args (change #'apply-propapp))
              :unapply (lambda-ignoring-args (change #'unapply-propapp))
              :args (cdr propapp))))
