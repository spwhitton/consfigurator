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
  NAME, call PROPAPPARGS to access them.  For passing a whole list of args on
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
                           (propappdesc (choose-propapp)))
                   :hostattrs (lambda (&rest args)
                                (declare (ignore args))
                                (propappattrs (choose-propapp)))
                   :apply (lambda (&rest args)
                            (declare (ignore args))
                            (propappapply (choose-propapp)))
                   :unapply (lambda (&rest args)
                              (declare (ignore args))
                              (propappunapply (choose-propapp))))))
     (setf (get ',name 'inline-combinator) t)))

(defun skip-property-restarts ()
  (loop for restart in (compute-restarts)
        when (eql 'skip-property (restart-name restart))
          collect restart))

;; There can be multiple SKIP-PROPERTY restarts established at once, and we
;; need this handler to invoke the one established right after we establish
;; this handler.
(defmacro with-skip-failed-changes (&body forms)
  (with-gensyms (old-restarts)
    `(let ((,old-restarts (skip-property-restarts)))
       (handler-bind ((failed-change
                        (lambda (c)
                          (with-indented-inform
                            (apply #'informat t
                                   (simple-condition-format-control c)
                                   (simple-condition-format-arguments c)))
                          ;; We can't just use NSET-DIFFERENCE and take the
                          ;; LASTCAR because NSET-DIFFERENCE provides no
                          ;; ordering guarantees.
                          (loop with chosen
                                for restart in (skip-property-restarts)
                                unless (member restart ,old-restarts)
                                  do (setq chosen restart)
                                finally (invoke-restart chosen)))))
         ,@forms))))

(define-function-property-combinator eseqprops (&rest propapps)
  (:retprop :type (collapse-types (mapcar #'propapptype propapps))
            :hostattrs (lambda () (mapc #'propappattrs propapps))
            :apply (lambda () (apply-and-print propapps))
            :unapply (lambda () (apply-and-print propapps t))))

(define-function-property-combinator seqprops (&rest propapps)
  (:retprop :type (collapse-types (mapcar #'propapptype propapps))
            :hostattrs (lambda () (mapc #'propappattrs propapps))
            :apply (lambda ()
                     (with-skip-failed-changes
                       (apply-and-print propapps)))
            :unapply (lambda ()
                       (with-skip-failed-changes
                         (apply-and-print propapps t)))))

(defmacro with-requirements (propapp &body requirements)
  "Apply PROPAPP only after applying each dependency in REQUIREMENTS.
Each item in REQUIREMENTS implicitly depends on the one preceding it, i.e., we
apply the elements of REQUIREMENTS in reverse order."
  `(eseqprops ,@(reverse requirements) ,propapp))

(define-function-property-combinator silent-seqprops (&rest propapps)
  (:retprop :type (collapse-types (mapcar #'propapptype propapps))
            :hostattrs (lambda () (mapc #'propappattrs propapps))
            :apply (lambda ()
                     (with-skip-failed-changes
                       (apply-and-print propapps nil t)))
            :unapply (lambda ()
                       (with-skip-failed-changes
                         (apply-and-print propapps t t)))))

(defun apply-and-print (propapps &optional unapply silent)
  (let ((buffer (make-array '(0) :element-type 'character
			         :fill-pointer 0 :adjustable t))
        (return-value :no-change)
        ;; Remove any null propapps because we don't want to print anything
        ;; for those, and applying them will do nothing.
        (propapps (remove nil (if unapply (reverse propapps) propapps))))
    (labels ((propapp-apply (propapp)
               (if unapply (propappunapply propapp) (propappapply propapp)))
             (announce-propapp-apply (propapp)
               (with-output-to-string (*standard-output* buffer)
                 (with-indented-inform
                   (propapp-apply propapp)))))
      (dolist (propapp propapps return-value)
        (let ((announce
                (and (not silent)
                     (or (> *consfigurator-debug-level* 2)
                         (not (get (get (car propapp) 'combinator)
                                   'inline-combinator)))
                     ;; We don't announce properties whose names begin with
                     ;; '%' and which have no description; these are typically
                     ;; DEFPROPs which exist only for use within a
                     ;; DEFPROPLIST/DEFPROPSPEC defining an exported property.
                     (not (and (< *consfigurator-debug-level* 3)
                               (char= #\% (char (symbol-name (car propapp)) 0))
                               (not (get (car propapp) 'desc))))))
              ;; Initialise to FAILED-CHANGE here so that if there is a
              ;; non-local exit from us we print "failed".  For example, if
              ;; the user or a combinator invokes a SKIP-PROPERTY restart
              ;; established further down the property call stack.
              (result 'failed-change))
          (unwind-protect
               (restart-case (setq result (if announce
                                              (announce-propapp-apply propapp)
                                              (propapp-apply propapp)))
                 (skip-property ()
                   :report (lambda (s)
                             (format s "Skip (~{~S~^ ~})"
                                     (cons (car propapp) (propappargs propapp))))
                   (signal 'skipped-properties)
                   'failed-change))
            (when (and (plusp (length buffer))
                       (or silent
                           (> *consfigurator-debug-level* 1)
                           (not (eql result :no-change))))
              (fresh-line)
              (princ buffer))
            (when announce
              (informat t "~&~@[~A :: ~]~@[~A ... ~]~A~%"
                        (get-hostname) (propappdesc propapp)
                        (case result
                          (:no-change     "ok")
                          ('failed-change "failed")
                          (t              "done")))))
          (setf (fill-pointer buffer) 0
                result (if (eql result 'failed-change) nil result))
          (unless (eql result :no-change)
            (setq return-value result)))))))

(define-function-property-combinator unapply (propapp)
  (destructuring-bind (psym . args) propapp
    (:retprop :type (proptype psym)
              :lambda (proplambda psym)
              :desc (lambda (&rest args)
                      (strcat "Unapply: " (apply #'propdesc psym args)))
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
  (:retprop :type (propapptype propapp)
            :desc (lambda () desc)
            :hostattrs (lambda (&rest args)
                         (declare (ignore args))
                         (propappattrs propapp))
            :apply (lambda (&rest args)
                     (declare (ignore args))
                     (propappapply propapp))
            :unapply (lambda (&rest args)
                       (declare (ignore args))
                       (propappunapply propapp))))

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
    (:retprop :type
              (collapse-types (propapptype propapp) (propapptype on-change))
              :desc (get prop 'desc)
              :hostattrs (lambda (&rest args)
                           (apply #'propattrs prop args)
                           (propappattrs on-change))
              :apply (lambda (&rest args)
                       (if (eql :no-change (apply #'propapply prop args))
                           :no-change
                           (propappapply on-change)))
              :unapply (lambda (&rest args)
                         (let ((result (apply #'propunapply prop args)))
                           (cond ((eql :no-change result) :no-change)
                                 (unapply (propappapply on-change))
                                 (t result))))
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
      `(reconnects. `((:as :to ,,user)) ,@properties)))

(defmacro with-flagfile (flagfile &body propapps)
  "Apply PROPAPPS unless FLAGFILE exists on the remote; after applying, create
FLAGFILE.
Useful to ensure that something is done just once.  Has the semantics that if
FLAGFILE exists, PROPAPPS are assumed to all be already applied."
  `(with-flagfile*
    ,flagfile
    ,(if (cdr propapps) `(eseqprops ,@propapps) (car propapps))))

(define-function-property-combinator with-flagfile* (flagfile propapp)
  (:retprop :type (propapptype propapp)
            :desc (get (car propapp) 'desc)
            :hostattrs (get (car propapp) 'hostattrs)
            :check (lambda-ignoring-args
                     (remote-exists-p flagfile))
            :apply (lambda-ignoring-args
                     (prog1 (propappapply propapp)
                       (mrun "mkdir" "-p"
                             (pathname-directory-pathname flagfile))
                       (mrun "touch" flagfile)))
            :unapply (lambda-ignoring-args
                       (prog1 (propappunapply propapp)
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
  (let* ((apply (loop for propapp in propapps
                      until (eql propapp :unapply) collect propapp))
         (unapply (member :unapply propapps))
         (apply-propapp
           (if (cdr apply) (apply #'eseqprops apply) (car apply)))
         (unapply-propapp (if (cddr unapply)
                              (apply #'eseqprops (cdr unapply))
                              (cadr unapply))))
    (if unapply
        (:retprop :type (collapse-propapp-types apply (cdr unapply))
                  :hostattrs (lambda-ignoring-args
                               (propappattrs apply-propapp)
                               ;; as in definition of UNAPPLY combinator
                               (with-preserve-hostattrs
                                 (propappattrs unapply-propapp)))
                  :apply (lambda-ignoring-args (propappapply apply-propapp))
                  :unapply (lambda-ignoring-args
                             (propappapply unapply-propapp)))
        apply-propapp)))
