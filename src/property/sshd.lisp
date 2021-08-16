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

;;;; Basic configuration

(defproplist installed :posix ()
  "Install an OpenSSH server."
  (:desc "OpenSSH server installed")
  (os:etypecase
      (debianlike (apt:installed "openssh-server"))))

(defprop configured :posix (&rest pairs)
  "Set key--value pairs in /etc/ssh/sshd_config."
  (:desc (format nil "sshd configured ~{~A ~A~^, ~}" pairs))
  (:apply
   (if (eql :no-change
            (apply #'file:contains-conf-space "/etc/ssh/sshd_config" pairs))
       :no-change
       (service:reloaded "sshd"))))

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
   (configured "PermitRootLogin" "prohibit-password"
               "PasswordAuthentication" "no")))


;;;; Host keys

(defprop has-host-public-key :posix (type public-key)
  "Records an SSH public key of type TYPE as identifying this host."
  (:desc #?"Has SSH host key of type ${type}")
  (:hostattrs (push-hostattrs 'host-public-key (cons type public-key))))

(defproplist has-host-key :posix (type public-key)
  "Installs the host key whose public part is PUBLIC-KEY and is of type TYPE.
The private key is obtained as an item of prerequisite data."
  (:desc #?"SSH host key of type ${type} installed")
  (has-host-public-key type public-key)
  (file:has-content (merge-pathnames (strcat "ssh_host_" type "_key.pub")
                                     #P"/etc/ssh/")
    public-key)
  (file:host-secret-uploaded (merge-pathnames (strcat "ssh_host_" type "_key")
                                              #P"/etc/ssh/")))

(defun get-host-public-keys (host &key short-hostname (aliases t)
                                    (ips t) additional-names)
  (let* ((host (preprocess-host host))
         (hostname (get-hostname host))
         (short (and short-hostname (list (get-short-hostname host))))
         (aliases (and aliases (get-hostattrs :aliases host)))
         (ips (and ips (append (get-hostattrs :ipv6 host)
                               (get-hostattrs :ipv4 host)))))
    (cons (format nil "~{~A~^,~}"
                  (cons hostname (append aliases short ips additional-names)))
          (mapcar #'cdr (get-hostattrs 'host-public-key host)))))
