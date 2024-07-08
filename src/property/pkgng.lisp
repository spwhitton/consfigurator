;;; Consfigurator -- Lisp declarative configuration management system

;;; Copyright (C) 2024  Sean Whitton <spwhitton@spwhitton.name>

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

(in-package :consfigurator.property.pkgng)
(named-readtables:in-readtable :consfigurator)

;;;; FreeBSD's pkg(8)

(defun mrun-pkg (&rest args)
  (let ((env '(:ASSUME_ALWAYS_YES "YES")))
    (when (get-connattr 'updatedp)
      (setq env (list* :REPO_AUTOUPDATE "NO" env)))
    (apply #'mrun :env env "pkg" args)))

(defun get-installed-packages ()
  (or (get-connattr 'installed-packages)
      (setf (get-connattr 'installed-packages)
            (mapcar #1~/^(\S+)-[^-]+?\s/ (lines (mrun-pkg "info" "-a"))))))

(defprop installed :posix (&rest packages)
  "Ensure all of the pkg(8) packages PACKAGES are installed."
  (:desc #?"pkg(8) installed @{packages}")
  (:hostattrs (os:required 'os:freebsd))
  (:check (subsetp packages (get-installed-packages) :test #'string=))
  (:apply (mrun-pkg :inform "install" packages)
          (setf (get-connattr 'updatedp) t
                ;; Reset Consfigurator's idea of what's installed, as we don't
                ;; know what additional dependencies were just installed.
                (get-connattr 'installed-packages) nil)))

(defprop deleted :posix (&rest packages)
  "Ensure all of the pkg(8) packages PACKAGES are removed."
  (:desc #?"pkg(8) removed @{packages}")
  (:hostattrs (os:required 'os:freebsd))
  (:check (null (intersection packages (get-installed-packages)
                              :test #'string=)))
  (:apply (mrun-pkg :inform "delete" packages)
          (setf (get-connattr 'installed-packages)
                (set-difference (get-connattr 'installed-packages) packages
                                :test #'string=))))

(defprop upgraded :posix ()
  (:desc "pkg(8) upgraded")
  (:hostattrs (os:required 'os:freebsd))
  (:check (prog1 (zerop (mrun-pkg :for-exit "upgrade" "-q" "-n"))
            (setf (get-connattr 'updatedp) t)))
  (:apply (mrun-pkg :inform "upgrade")
          ;; Reset Consfigurator's idea of what's installed, as some packages
          ;; may have been removed.
          (setf (get-connattr 'installed-packages) nil)))

(defprop autoremoved :posix ()
  (:desc "pkg(8) removed automatically installed packages")
  (:hostattrs (os:required 'os:freebsd))
  (:check (null (lines (mrun-pkg "autoremove" "-q" "-n"))))
  (:apply (mrun-pkg :inform "autoremove")
          ;; Reset Consfigurator's idea of what's installed, as we don't know
          ;; what was just removed.
          (setf (get-connattr 'installed-packages) nil)))

(defprop cache-cleaned :posix ()
  "Remove superseded & obsolete data from the local package cache."
  (:desc "pkg(8) cache cleaned")
  (:hostattrs (os:required 'os:freebsd))
  (:apply (mrun-pkg "clean") :no-change))

(defprop cache-emptied :posix ()
  "Completely empty the local package cache."
  (:desc "pkg(8) cache emptied")
  (:hostattrs (os:required 'os:freebsd))
  (:apply (mrun-pkg "clean" "-a") :no-change))
