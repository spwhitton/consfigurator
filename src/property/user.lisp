;;; Consfigurator -- Lisp declarative configuration management system

;;; Copyright (C) 2021  David Bremner <david@tethera.net>

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

(in-package :consfigurator.property.user)
(named-readtables:in-readtable :interpol-syntax)

(defprop has-account :posix (user-name)
  "ensure there is an account for USER-NAME."
  (:desc #?"account for ${user-name}")
  (:check
   (user-exists user-name))
  (:apply
   (assert-euid-root)
   (run "useradd" "-m" user-name)))

(defun user-exists (user-name)
  (zerop (run :for-exit "getent" "passwd" user-name)))