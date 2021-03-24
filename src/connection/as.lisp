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

(in-package :consfigurator.connection.as)
(named-readtables:in-readtable :consfigurator)

;; currently we only check whether we're root, but, for example, on Linux, we
;; might have a CAP_* which lets us setuid as non-root
(defun can-setuid ()
  (zerop (foreign-funcall "geteuid" :int)))

(defmethod establish-connection ((type (eql :as)) remaining &key to)
  "Establish a :SETUID or :SUDO connection to another user account, depending
on whether it is possible to establish a :SETUID connection.

This connection type does not support sudo with a password -- it is designed
to be used as root."
  (if (and (lisp-connection-p)
           (can-setuid)
           (can-probably-fork))
      (establish-connection :setuid remaining :to to)
      (establish-connection :sudo remaining :user to)))
