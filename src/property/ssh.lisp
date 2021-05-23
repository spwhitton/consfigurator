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

(in-package :consfigurator.property.ssh)
(named-readtables:in-readtable :consfigurator)

(defprop authorized-keys :posix (&rest keys)
  "Permits using KEYS to SSH in as the current user."
  (:desc (declare (ignore keys))
         (strcat (get-connattr :remote-user) " has authorized_keys"))
  (:apply
   (file:directory-exists ".ssh")
   (apply #'file:contains-lines ".ssh/authorized_keys" keys))
  (:unapply
   (apply #'file:lacks-lines ".ssh/authorized_keys" keys)))
