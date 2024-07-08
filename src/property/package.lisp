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
;;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

(in-package :consfigurator.property.package)
(named-readtables:in-readtable :consfigurator)

(define-constant +consfigurator-system-dependencies+
    '(:apt ("build-essential" "libacl1-dev" "libcap-dev"))
  :test #'equal)

(defgeneric %command (package-manager)
  (:documentation
   "Returns a command which, if found on PATH, indicates that the system package
manager identified by PACKAGE-MANAGER is available."))

(defmethod %command ((package-manager (eql :apt)))
  "apt-get")

(defmethod %command ((package-manager (eql :pkgng)))
  "pkg")

(defgeneric %installed (package-manager packages)
  (:documentation
   "Install each of PACKAGES using the system package manager identified by
PACKAGE-MANAGER.

Implementations should not fail just because we are not root, or otherwise
privileged, if the package is already installed."))

;; Call APPLY-PROPAPP directly because we want the :CHECK subroutine run, but
;; it doesn't make sense to run :HOSTATTRS because *HOST* doesn't necessarily
;; correspond to the host on which we're attempting to install packages.

(defmethod %installed ((package-manager (eql :apt)) packages)
  (apply-propapp `(apt:installed ,@packages)))

(defmethod %installed ((package-manager (eql :pkgng)) packages)
  (apply-propapp `(pkgng:installed ,@packages)))

(define-simple-error package-manager-not-found (aborted-change))

(defprop installed :posix
    (package-manager &rest package-lists &aux package-list)
  "Attempt to use a system package manager to install system packages as
specified by PACKAGE-LISTS.  If PACKAGE-MANAGER, a keyword, use that
particular package manager; otherwise, see what we can find on PATH.

Each of PACKAGE-LISTS is a plist where the keys identify package managers, and
where the values are lists of package names to install using that package
manager.  See PACKAGE:+CONSFIGURATOR-SYSTEM-DEPENDENCIES+ for an example.

This property should not typically be applied to hosts.  It is preferable to
use an operating system-specific property, such as APT:INSTALLED.  This
property exists because in a few cases it is necessary to install packages
where there is no known-valid HOST value for the machine upon which we need to
install packages, and thus we cannot infer what package manager to use from
the host's OS, and must fall back to seeing what's on PATH.

In particular, when starting up a remote Lisp image when the REMAINING
argument to ESTABLISH-CONNECTION is non-nil, we might be starting up Lisp on a
machine other than the one to be deployed and we do not have HOST values for
intermediate hops.  Another case is INSTALLED:CLEANLY-INSTALLED-ONCE;
regardless of REMAINING, the initial OS might be the one we will replace, not
the declared OS for the host."
  (:apply
   (dolist (list package-lists)
     (doplist (k v list)
       (dolist (p (ensure-cons v))
         (push p (getf package-list k)))))
   (loop with reversed
         for (k v) on package-list by #'cddr
         do (push v reversed) (push k reversed)
         finally (setq package-list reversed))
   (if package-manager
       (return-from installed
         (%installed package-manager (getf package-list package-manager)))
       (doplist (package-manager packages package-list)
         (when (remote-executable-find (%command package-manager))
           (return-from installed (%installed package-manager packages)))))
   (package-manager-not-found
    "Could not find any package manager on PATH with which to install ~S."
    package-list)))
