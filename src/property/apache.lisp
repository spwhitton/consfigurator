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

(in-package :consfigurator.property.apache)
(named-readtables:in-readtable :consfigurator)

(defproplist installed :posix ()
  (:desc "Apache installed")
  (os:etypecase
    (debianlike (apt:installed "apache2"))))

(defproplist reloaded :posix ()
  (:desc "Apache reloaded")
  (service:reloaded "apache2"))

(defprop %mod-enabled :posix (name)
  (:hostattrs (os:required 'os:debianlike))
  (:check (zerop (mrun :for-exit "a2query" "-q" "-m" name)))
  (:apply (mrun "a2enmod" "--quiet" name))
  (:unapply (mrun "a2dismod" "--quiet" name)))

(defproplist mod-enabled :posix (name)
  (:desc #?"Apache module ${name} enabled")
  (installed)
  (on-change (%mod-enabled name)
    (reloaded)))

(defprop %conf-enabled :posix (name)
  (:hostattrs (os:required 'os:debianlike))
  (:check (zerop (mrun :for-exit "a2query" "-q" "-c" name)))
  (:apply (mrun "a2enconf" "--quiet" name))
  (:unapply (mrun "a2disconf" "--quiet" name)))

(defproplist conf-enabled :posix (name)
  (:desc #?"Apache configuration ${name} enabled")
  (installed)
  (on-change (%conf-enabled name)
    (reloaded)))
