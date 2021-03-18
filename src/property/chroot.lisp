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

(defprop %debootstrapped :posix (options root host)
  "Bootstrap The Universal Operating System into ROOT using debootstrap(1)."
  (:check
   (declare (ignore options host))
   ;; check whether a previous debootstrap failed partway through
   (if (test "-d" (merge-pathnames "debootstrap/"
				   (ensure-directory-pathname root)))
       (progn (mrun "rm" "-rf" root) nil)
       (test "-d" root))))

(defpropspec %os-bootstrapper-installed :posix (host)
  `(os:host-typecase ,host
     (debian
      (os:typecase
	(debian (apt:installed "debootstrap"))))))

(defpropspec %os-bootstrapped :posix (options root host)
  "Bootstrap OS into ROOT, e.g. with debootstrap(1)."
  (once-only (host)
    `(os:host-typecase ,host
       (debian (%debootstrapped ,options ,root ,host)))))

(defproplist os-bootstrapped :posix
  (options root properties
	   &aux (host (preprocess-host (make-host :propspec properties))))
  "Bootstrap an OS into ROOT and apply PROPERTIES.
OPTIONS is a value to pass to the OS-specific bootstrapping property."
  (:desc
   (declare (ignore options properties))
   #?"Built chroot ${root}")
  (%os-bootstrapper-installed host)
  (%os-bootstrapped options root host)
  (deploys `((:chroot :into ,root)) host))
