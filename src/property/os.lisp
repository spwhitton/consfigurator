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

(in-package :consfigurator.property.os)
(named-readtables:in-readtable :interpol-syntax)

;;;; Basic OS types

(defclass unixlike () ())

(defclass linux (unixlike)
  ((architecture
    :initarg :arch :reader linux-architecture
    :documentation
    "Keyword whose name is Debian's name for this architecture, e.g. :AMD64")))

(defclass debianlike (linux) ())

(defclass debian (debianlike)
  ((suite :initarg :suite
	  :reader debian-suite
	  :initform (error "Must provide suite"))))

(defmethod print-object ((os debian) stream)
  (format stream "#.~S" `(make-instance 'debian
					:arch ,(linux-architecture os)
					:suite ,(debian-suite os)))
  os)

(defclass debian-stable (debian) ())

(defprop debian-stable :posix (suite architecture)
  (:desc
   (declare (ignore architecture))
   #?{Host is Debian "${suite}"})
  (:hostattrs
   (push-hostattrs :os
		   (make-instance 'debian-stable
				  :arch architecture :suite suite))))

(defclass debian-testing (debian)
  ((suite :initform "testing")))

(defprop debian-testing :posix (architecture)
  (:desc
   (declare (ignore architecture))
   "Host is Debian testing")
  (:hostattrs
   (push-hostattrs :os
		   (make-instance 'debian-testing
				  :arch architecture))))

(defclass debian-unstable (debian)
  ((suite :initform "unstable")))

(defprop debian-unstable :posix (architecture)
  (:desc
   (declare (ignore architecture))
   "Host is Debian unstable")
  (:hostattrs
   (push-hostattrs :os
		   (make-instance 'debian-unstable
				  :arch architecture))))

(defmethod debian-architecture ((os linux))
  "Return a string representing the architecture of OS as used by Debian."
  (string-downcase (symbol-name (linux-architecture os))))


;;;; Property combinators

(define-function-property-combinator os-typecase* (host &rest cases)
  (flet ((choose-propapp ()
	   (or (loop with os = (class-of (if host
					     (car (getf (hostattrs host) :os))
					     (get-hostattrs-car :os)))
		     for (type propapp) on cases by #'cddr
		     when (subtypep os type) return propapp)
	       (inapplicable-property
		"Host's OS ~S fell through OS:TYPECASE."
		(class-of (get-hostattrs-car :os))))))
    (:retprop :type (collapse-types (loop for propapp in (cdr cases) by #'cddr
					  collect (propapptype propapp)))
	      :desc (lambda (&rest args)
		      (declare (ignore args))
		      (propappdesc (choose-propapp)))
	      :apply (lambda (&rest args)
		       (declare (ignore args))
		       (propappapply (choose-propapp)))
	      :unapply (lambda (&rest args)
			 (declare (ignore args))
			 (propappunapply (choose-propapp))))))

(defmacro typecase (&body cases)
  `(host-typecase nil ,@cases))

(defmacro host-typecase (host &body cases)
  `(os-typecase*
    ,host
    ,@(loop for case in cases
	    collect `',(intern (symbol-name (car case))
			       (find-package :consfigurator.property.os))
	    collect (cadr case))))


;;;; Utilities

(defun required (type)
  "Error out if the OS of the host being deployed is not of type TYPE.

Used in property :HOSTATTRS subroutines."
  (let ((os (class-of (get-hostattrs-car :os))))
    (unless (and os (subtypep os type))
      (inapplicable-property #?"Property requires OS of type ${type}"))))

(defun supports-arch-p (os arch)
  "Can binaries of type ARCH run on OS?"
  (cl:typecase os
    (debian (or (eq (linux-architecture os) arch)
		(member arch (assoc (linux-architecture os)
				    '((:amd64 :i386)
				      (:i386  :amd64))))))))
