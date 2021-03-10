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

(in-package :consfigurator.property.apt)
(named-readtables:in-readtable :interpol-syntax)


;;;; Macros

(defmacro with-maybe-update (form)
  `(handler-case ,form
     (run-failed ()
       (apt-get :princ "update")
       ,form)))


;;;; Properties

(defprop installed :posix (&rest packages)
  "Ensure all of the apt packages PACKAGES are installed."
  (:desc #?"apt installed @{packages}")
  (:hostattrs
   (declare (ignore packages))
   (os:required 'os:debianlike))
  (:check
   (all-installed-p packages))
  (:apply
   (with-maybe-update (apt-get :princ "-y" "install" packages))))

(defprop removed :posix (&rest packages)
  "Ensure all of the apt packages PACKAGES are removed."
  (:desc #?"apt removed @{packages}")
  (:hostattrs
   (declare (ignore packages))
   (os:required 'os:debianlike))
  (:check
   (none-installed-p packages))
  (:apply
   (apt-get :princ "-y" "remove" packages)))


;;;; Reports on installation status

(defun apt-cache-policy (packages)
  (runlines :env '(:LANG "C") "apt-cache" "policy" packages))

(define-constant apt-cache-policy-installed #?/^\s+Installed:\s+(?!\(none\))/
  :test #'string=)

(defun all-installed-p (packages)
  (loop with n = 0
	for line in (apt-cache-policy packages)
	when (re:scan apt-cache-policy-installed line)
	  do (incf n)
	finally (return (= n (length packages)))))

(defun none-installed-p (packages)
  (loop for line in (apt-cache-policy packages)
	never (re:scan apt-cache-policy-installed line)))


;;;; Utilities

(defun apt-get (&rest args)
  (apply #'run
	 :env '(:DEBIAN_FRONTEND "noninteractive"
		:APT_LISTCHANGES_FRONTEND "none")
	 "apt-get" args))
