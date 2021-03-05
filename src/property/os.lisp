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

(defun required (type)
  "Error out if the OS of the host being deployed is not of type TYPE.

Used in property :HOSTATTRS subroutines."
  (let ((os (class-of (car (get-hostattrs :os)))))
    (unless (and os (subtypep os type))
      (error 'inapplicable-property
	     :text #?"Property requires OS of type ${type}"))))

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
