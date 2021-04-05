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
(named-readtables:in-readtable :consfigurator)


;;;; Static definitions

(defmacro with-maybe-update (form)
  `(handler-case ,form
     (run-failed ()
       (apt-get :inform "update")
       ,form)))

(define-constant +sections+ '("main" "contrib" "non-free") :test #'equal)

(define-constant +noninteractive-env+ '(:DEBIAN_FRONTEND "noninteractive"
                                        :APT_LISTCHANGES_FRONTEND "none")
  :test #'equal)


;;;; Properties

(defprop installed :posix (&rest packages)
  "Ensure all of the apt packages PACKAGES are installed."
  (:desc #?"apt installed @{packages}")
  (:hostattrs
   (declare (ignore packages))
   (os:required 'os:debianlike))
  (:check
   (apply #'all-installed-p packages))
  (:apply
   (with-maybe-update (apt-get :inform "-y" "install" packages))))

(defprop removed :posix (&rest packages)
  "Ensure all of the apt packages PACKAGES are removed."
  (:desc #?"apt removed @{packages}")
  (:hostattrs
   (declare (ignore packages))
   (os:required 'os:debianlike))
  (:check
   (apply #'none-installed-p packages))
  (:apply
   (apt-get :inform "-y" "remove" packages)))

(defproplist service-installed-running :posix (package)
  "Where PACKAGE installs a service named PACKAGE, ensure it is installed and
running.

E.g. (APT:SERVICE-INSTALLED-RUNNING \"apache2\")."
  (:desc #?"${package} installed and running")
  (installed package)
  (service:running package))

(defprop mirror :posix (uri)
  (:desc #?"${uri} apt mirror selected")
  (:hostattrs
   (pushnew-hostattrs :apt.mirror uri)))

(defproplist uses-parent-mirror :posix ()
  (:desc #?"Uses parent's apt mirror")
  (mirror (get-parent-hostattrs-car :apt.mirror)))

(defprop proxy :posix (uri)
  (:desc #?"${uri} apt proxy selected")
  (:hostattrs
   (pushnew-hostattrs :apt.proxy uri))
  (:apply
   (file:has-content "/etc/apt/apt.conf.d/20proxy"
                     (format nil "Acquire::HTTP::Proxy \"~A\";~%" uri))))

(defproplist uses-parent-proxy :posix ()
  (:desc #?"Uses parent's apt proxy")
  (proxy (get-parent-hostattrs-car :apt.proxy)))

(defproplist uses-local-cacher :posix ()
  (:desc "apt uses local apt cacher")
  (service-installed-running "apt-cacher-ng")
  (proxy "http://localhost:3142"))

(defun get-mirrors ()
  (or (get-hostattrs :apt.mirror) (call-with-os #'get-default-mirrors)))

(defmethod get-default-mirrors ((os os:debian))
  '("http://deb.debian.org/debian"))

(defprop standard-sources.list :posix ()
  (:desc "Standard sources.list")
  (:apply
   (file:has-content "/etc/apt/sources.list"
     (call-with-os #'standard-sources-for))))

(defmethod standard-sources-for ((os os:debian))
  (let* ((suite (os:debian-suite os))
         (archive (mapcar (lambda (m) (list* m suite +sections+))
                          (get-mirrors)))
         (security-suite (if (memstring= suite '("stretch" "jessie" "buster"))
                             #?"${suite}/updates"
                             #?"${suite}-security"))
         (security (and (not (subtypep (type-of os) 'os:debian-unstable))
                        (list
                         (list* "http://security.debian.org/debian-security"
                               security-suite +sections+)))))
    (mapcan (lambda (l) (list #?"deb @{l}" #?"deb-src @{l}"))
            (nconc archive security))))


;;;; Reports on installation status

(defun apt-cache-policy (packages)
  (runlines :env '(:LANG "C") "apt-cache" "policy" packages))

(define-constant apt-cache-policy-installed #?/^\s+Installed:\s+(?!\(none\))/
  :test #'string=)

(defun all-installed-p (&rest packages)
  (loop with n = 0
        for line in (apt-cache-policy packages)
        when (re:scan apt-cache-policy-installed line)
          do (incf n)
        finally (return (= n (length packages)))))

(defun none-installed-p (&rest packages)
  (loop for line in (apt-cache-policy packages)
        never (re:scan apt-cache-policy-installed line)))


;;;; Utilities

(defun apt-get (&rest args)
  (apply #'run :env +noninteractive-env+ "apt-get" args))
