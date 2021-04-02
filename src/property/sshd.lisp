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

(in-package :consfigurator.property.sshd)
(named-readtables:in-readtable :consfigurator)

(defproplist installed :posix ()
  "Install an OpenSSH server."
  (:desc "OpenSSH server installed")
  (os:typecase
      (debianlike (apt:installed "openssh-server"))))

(defprop configured :posix (&rest pairs)
  "Set key--value pairs in /etc/ssh/sshd_config."
  (:desc (format nil "sshd configured ~{~A ~A~^, ~}" pairs))
  (:apply
   (apply #'file:contains-conf-space "/etc/ssh/sshd_config" pairs)))

(defprop no-passwords :posix ()
  "Configure SSH to disallow password logins.
To prevent lockouts, also enables logging in as root with an SSH key, and
refuses to proceed if root has no authorized_keys."
  (:desc "SSH passwords disabled")
  (:apply
   (assert-euid-root)
   (unless (and (remote-exists-p ".ssh/authorized_keys")
                (plusp (length (readfile ".ssh/authorized_keys"))))
     (failed-change "root has no authorized_keys"))
   (configured "PermitRootLogin" "without-password"
               "PasswordAuthentication" "no")))
