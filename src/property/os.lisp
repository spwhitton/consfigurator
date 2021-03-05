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
  (:hostattrs
   (push-hostattrs :os
		   (make-instance 'debian-stable
				  :architecture architecture :suite suite))))

(defclass debian-testing (debian)
  ((suite :initform "testing")))

(defprop debian-testing :posix (architecture)
  (:hostattrs
   (push-hostattrs :os
		   (make-instance 'debian-testing
				  :architecture architecture))))

(defclass debian-unstable (debian)
  ((suite :initform "unstable")))

(defprop debian-unstable :posix (architecture)
  (:hostattrs
   (push-hostattrs :os
		   (make-instance 'debian-unstable
				  :architecture architecture))))
