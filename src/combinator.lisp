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

(defmacro with-skip-failed-changes (&body forms)
  `(handler-bind ((failed-change
                    (lambda (c)
                      (with-indented-inform
                        (informat t
                                  (simple-condition-format-control c)
                                  (simple-condition-format-arguments c)))
                      (invoke-restart 'skip-property))))
     ,@forms))

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
                       (mapc #'propappapply propapps)))
            :unapply (lambda ()
                       (with-skip-failed-changes
                         (mapc #'propappunapply (reverse propapps))))))

;; note that the :FAILED-CHANGE value is only used within this function and
;; should not be returned by property subroutines, per the spec
(defun apply-and-print (propapps &optional unapply)
  (let ((buffer (make-array '(0) :element-type 'base-char
			         :fill-pointer 0 :adjustable t))
        (return-value :no-change)
        ;; Remove any null propapps because we don't want to print anything
        ;; for those, and applying them will do nothing.
        (propapps
          (remove-if #'null (if unapply (reverse propapps) propapps))))
    (labels ((propapp-apply (propapp)
               (if unapply (propappunapply propapp) (propappapply propapp)))
             (announce-propapp-apply (propapp)
               (setf (fill-pointer buffer) 0)
               (with-output-to-string (*standard-output* buffer)
                 (with-indented-inform
                   (propapp-apply propapp)))))
      (dolist (propapp propapps return-value)
        (let ((announce (or (> *consfigurator-debug-level* 1)
                            (not (get (get (car propapp) 'combinator)
                                      'inline-combinator))))
              result)
          (unwind-protect-in-parent
              ;; TODO Nested combinators can mean that we establish this
              ;; restart more than once, and they all appear in the debugger
              ;; without any way to distinguish them.  Perhaps we can use the
              ;; :TEST argument to RESTART-CASE such that only the
              ;; innermost(?) skip option appears.
              (setq result (restart-case (if announce
                                             (announce-propapp-apply propapp)
                                             (propapp-apply propapp))
                             (skip-property () :failed-change)))
            (when (and (plusp (length buffer))
                       (or (> *consfigurator-debug-level* 1)
                           (not (eql result :no-change))))
              (fresh-line)
              (princ buffer)))
          (when announce
            (informat t "~&~@[~A :: ~]~@[~A ... ~]~A~%"
                      (get-hostname) (propappdesc propapp)
                      (case result
                        (:no-change     "ok")
                        (:failed-change "failed")
                        (t              "done"))))
          (unless (or (null return-value) (eql result :no-change))
            (setq return-value nil)))))))

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
              :apply (get psym 'unapply)
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
  "If applying PROPAPP makes a change, also apply each of of the propapps
ON-CHANGE in order."
  `(on-change* ,propapp ,@on-change))

(define-function-property-combinator on-change* (propapp &rest propapps)
  (:retprop :type (collapse-types (propapptype propapp)
                                  (mapcar #'propapptype propapps))
            :desc (get (car propapp) 'desc)
            :hostattrs (lambda (&rest args)
                         (apply #'propattrs (car propapp) args)
                         (mapc #'propappattrs propapps))
            :apply (lambda (&rest args)
                     (if (eql :no-change
                              (propappapply (cons (car propapp) args)))
                         :no-change
                         (dolist (propapp propapps)
                           (propappapply propapp))))
            :unapply (lambda (&rest args)
                       (if (eql :no-change
                                (propappunapply (cons (car propapp) args)))
                           :no-change
                           (dolist (propapp (reverse propapps))
                             (propappunapply propapp))))
            :args (cdr propapp)))

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
