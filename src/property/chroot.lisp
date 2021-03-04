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

(in-package :consfigurator.property.chroot)
(named-readtables:in-readtable :interpol-syntax)

(defgeneric os-bootstrap (host root &key)
  (:documentation
   "Bootstrap HOST's OS into ROOT, e.g. with debootstrap(1)."))

(defproplist os-bootstrapped :posix
    (options root properties &aux (host (make-host :props properties)))
  (:desc #?"Built chroot ${root}")
  (%os-bootstrapped options root host)
  (deploys `((:chroot :into ,root)) host))

(defprop %os-bootstrapped :posix (options root host)
  (:check
   (declare (ignore options host))
   (test "-d" root))
  (:apply
   (apply #'os-bootstrap host root options)))
