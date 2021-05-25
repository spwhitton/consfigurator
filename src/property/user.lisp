;;; Consfigurator -- Lisp declarative configuration management system

;;; Copyright (C) 2021  David Bremner <david@tethera.net>
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

(in-package :consfigurator.property.user)
(named-readtables:in-readtable :consfigurator)

(defprop has-account :posix (username)
  "Ensure there is an account for USERNAME.
Note that this uses getent(1) and so is not strictly POSIX-compatible."
  (:desc #?"Account for ${username}")
  (:check
   (user-exists username))
  (:apply
   (assert-euid-root)
   (mrun "useradd" "-m" username)))

(defprop has-login-shell :posix (username shell)
  "Ensures that USERNAME has login shell SHELL.
Note that this uses getent(1) and so is not strictly POSIX-compatible."
  (:desc #?"${username} has login shell ${shell}")
  (:check
   (string= (passwd-entry 6 username) shell))
  (:apply
   (file:contains-lines "/etc/shells" shell)
   (mrun "chsh" "--shell" shell username)))

(defun passwd-entry (n username-or-uid)
  "Get the nth entry in the getent(1) output for USERNAME-OR-UID.
Note that getent(1) is not specified in POSIX so use of this function makes
properties not strictly POSIX-compatible."
  (let ((u (etypecase username-or-uid
	     (string username-or-uid)
	     (number (write-to-string username-or-uid)))))
    (nth n (split-string (stripln (mrun "getent" "passwd" u))
			 :separator ":"))))

(defun user-exists (username)
  (zerop (mrun :for-exit "getent" "passwd" username)))
