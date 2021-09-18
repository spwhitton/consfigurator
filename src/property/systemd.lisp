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

(in-package :consfigurator.property.systemd)
(named-readtables:in-readtable :consfigurator)

(defun systemctl (fn user &rest args &aux (args (cons "systemctl" args)))
  (apply fn (if user (apply #'systemd--user args) args)))

(defprop started :posix (service &optional user)
  (:desc #?"systemd service ${service} started")
  (:check (zerop (systemctl #'mrun user :for-exit "is-active" service)))
  (:apply (systemctl #'mrun user "start" service)))

(defprop stopped :posix (service &optional user)
  (:desc #?"systemd service ${service} stopped")
  (:check (plusp (systemctl #'mrun user :for-exit "is-active" service)))
  (:apply (systemctl #'mrun user "stop" service)))

(defprop enabled :posix (service &optional user)
  (:desc #?"systemd service ${service} enabled")
  (:check (zerop (systemctl #'mrun user :for-exit "is-enabled" service)))
  (:apply (systemctl #'mrun user "enable" service)))

(defprop disabled :posix (service &optional user)
  (:desc #?"systemd service ${service} disabled")
  (:check
   (let ((status
           (stripln
            (systemctl #'run user :may-fail "is-enabled" service))))
     (or (string-prefix-p "linked" status)
         (string-prefix-p "masked" status)
         (memstring=
          status '("static" "disabled" "generated" "transient" "indirect")))))
  (:apply (systemctl #'mrun user "disable" service)))

(defprop masked :posix (service &optional user)
  (:desc #?"systemd service ${service} masked")
  (:check
   (string-prefix-p
    "masked"
    (systemctl #'run user :may-fail "is-enabled" service)))
  (:apply (systemctl #'mrun user "mask" service))
  (:unapply (systemctl #'mrun user "unmask" service)))

(defprop lingering-enabled :posix (user)
  (:desc #?"User lingering enable for ${user}")
  (:check (memstring= "Linger=yes" (runlines "loginctl" "show-user" user)))
  (:apply (mrun "loginctl" "enable-linger" user))
  (:unapply (mrun "loginctl" "disable-linger" user)))
