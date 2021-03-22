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
                  (apply #'setprop psym setprop-args)
                  (return-from ,name (list* psym args)))))
         ,@forms))))

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
  (dolist (pa (if unapply (reverse propapps) propapps))
    (let* ((result (restart-case
                       (with-indented-inform
                         (if unapply (propappunapply pa) (propappapply pa)))
                     (skip-property () :failed-change)))
           (status (case result
                     (:no-change     "ok")
                     (:failed-change "failed")
                     (t              "done"))))
      (informat t "~&~@[~A :: ~]~@[~A ... ~]~A~%"
                (get-hostname) (propappdesc pa) status))))

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

(defmacro on-change (propapp &body on-change)
  "If applying PROPAPP makes a change, also apply each of of the propapps
ON-CHANGE in order."
  `(on-change* ,propapp ,@on-change))

(define-function-property-combinator on-change* (propapp &rest propapps)
  (:retprop :type (collapse-types (propapptype propapp)
                                  (mapcar #'propapptype propapps))
            :desc (get (car propapp) 'desc)
            :hostattrs (lambda (&rest args)
                         (apply #'propattrs (car propapp) args))
            :apply (lambda (&rest args)
                     (unless (eq (propappapply (cons (car propapp) args))
                                 :no-change)
                       (dolist (propapp propapps)
                         (propappapply propapp))))
            :unapply (lambda (&rest args)
                       (unless (eq (propappunapply (cons (car propapp) args))
                                   :no-change)
                         (dolist (propapp (reverse propapps))
                           (propappunapply propapp))))
            :args (cdr propapp)))
