;;; Consfigurator -- Lisp declarative configuration management system

;;; Copyright (C) 2021-2022  Sean Whitton <spwhitton@spwhitton.name>

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

(in-package :consfigurator.property.os)
(named-readtables:in-readtable :consfigurator)

;;;; Basic OS types

(defclass unixlike () ())

(defclass linux (unixlike) ())

(defprop linux :posix ()
  (:desc "Host kernel is Linux")
  (:hostattrs (push-hostattr :os (make-instance 'linux))))

(define-simple-print-object linux)

(defclass debianlike (linux) ())

(defclass debian (debianlike)
  ((architecture
    :initarg :arch :reader debian-architecture
    :documentation
    "Keyword whose name is Debian's name for this architecture, e.g. :AMD64")
   (suite :initarg :suite
          :reader debian-suite
          :initform (error "Must provide suite"))))

(define-simple-print-object debian)

(defclass debian-stable (debian) ())

(defprop debian-stable :posix (suite architecture)
  (:desc
   (declare (ignore architecture))
   #?{Host is Debian "${suite}"})
  (:hostattrs
   (push-hostattr :os
                  (make-instance 'debian-stable
                                 :arch architecture :suite suite))))

(defclass debian-testing (debian)
  ((suite :initform "testing")))

(defprop debian-testing :posix (architecture)
  (:desc
   (declare (ignore architecture))
   "Host is Debian testing")
  (:hostattrs
   (push-hostattr :os
                  (make-instance 'debian-testing
                                 :arch architecture))))

(defclass debian-unstable (debian)
  ((suite :initform "unstable")))

(defprop debian-unstable :posix (architecture)
  (:desc
   (declare (ignore architecture))
   "Host is Debian unstable")
  (:hostattrs
   (push-hostattr :os
                  (make-instance 'debian-unstable
                                 :arch architecture))))

(defclass debian-experimental (debian)
  ((suite :initform "experimental")))

(defmethod debian-architecture-string ((os debian))
  "Return a string representing the architecture of OS as used by Debian."
  (string-downcase (symbol-name (debian-architecture os))))


;;;; Property combinators

(defun cases-type (cases)
  (combine-propapp-types (loop for pa in (cdr cases) by #'cddr collect pa)))

(defun case-host (host fn)
  (funcall fn (if host (get-hostattrs-car :os host) (get-hostattrs-car :os))))

(defun case-choose (host cases reader pred &optional default)
  (loop with slot = (case-host host reader)
        for (case propapp) on cases by #'cddr
        when (or (and default (eql case t))
                 (funcall pred slot case))
          return propapp))

(defmacro define-host-case-combinators
    (name ename reader pred convert-key error-control)
  (with-gensyms (host cases key forms)
    (let ((case* (symbolicate name 'case*))
          (ecase* (symbolicate ename 'case*))
          (flatten `(loop for (,key . ,forms) in ,cases
                          collect (funcall ,convert-key ,key)
                          collect (if (cdr ,forms)
                                      `(eseqprops ,@,forms)
                                      (car ,forms)))))
      `(progn
         (define-choosing-property-combinator ,case* (host &rest cases)
           :type (cases-type cases)
           :choose (case-choose host cases ,reader ,pred t))

         (define-choosing-property-combinator ,ecase* (host &rest cases)
           :type (cases-type cases)
           :choose
           (or (case-choose host cases ,reader ,pred)
               (inapplicable-property ,error-control (case-host host ,reader))))

         (defmacro ,(symbolicate name 'case) (&body ,cases)
           `(,',case* nil ,@,flatten))

         (defmacro ,(symbolicate ename 'case) (&body ,cases)
           `(,',ecase* nil ,@,flatten))

         (defmacro ,(symbolicate 'host- name 'case) (,host &body ,cases)
           `(,',case* ,,host ,@,flatten))

         (defmacro ,(symbolicate 'host- ename 'case) (,host &body ,cases)
           `(,',ecase* ,,host ,@,flatten))))))

(define-host-case-combinators type etype
  #'class-of #'subtypep
  (lambda (key)
    `',(intern (symbol-name key)
               (find-package :consfigurator.property.os)))
  "Host's OS ~S fell through OS:ETYPECASE.")

(define-host-case-combinators debian-suite- debian-suite-e
  #'debian-suite #'string= #'identity
  "Host's Debian suite ~S fell through OS:DEBIAN-SUITE-ECASE.")


;;;; Utilities

(defun required (type)
  "Error out if the OS of the host being deployed is not of type TYPE.

Used in property :HOSTATTRS subroutines."
  (let ((os (class-of (get-hostattrs-car :os))))
    (unless (and os (subtypep os type))
      (inapplicable-property #?"Property requires OS of type ${type}"))))

(defgeneric supports-arch-p (target-os binary-os)
  (:documentation "Can binaries for BINARY-OS run on TARGET-OS?"))

(defmethod supports-arch-p ((target-os debian) (binary-os debian))
  (let ((target (debian-architecture target-os))
        (binary (debian-architecture binary-os)))
    (or (eq target binary)
        (member binary (assoc target '((:amd64 :i386)
                                       (:i386  :amd64)))))))
