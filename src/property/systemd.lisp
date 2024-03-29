;;; Consfigurator -- Lisp declarative configuration management system

;;; Copyright (C) 2021-2022  Sean Whitton <spwhitton@spwhitton.name>

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

(in-package :consfigurator.property.systemd)
(named-readtables:in-readtable :consfigurator)

;;; Using systemctl(1), we cannot enable or disable user units unless the
;;; user's service manager is actually running, and using loginctl(1), we
;;; cannot enable or disable user lingering unless systemd is PID 1.
;;;
;;; Possibly we should manually create the symlinks in the former case, and
;;; touch and delete the file under /var/lib/systemd/linger in the latter
;;; case, so that more configuration is applicable to unbooted chroots.

;;; We might have a combinator which implies ':user-instance t' for any of the
;;; following properties it contains.  The idea would be that within a
;;; sequence of properties you probably want to affect only either the system
;;; daemon or the user instance.

(defun systemctl (fn user &rest args &aux (args (cons "systemctl" args)))
  (apply fn (if user (systemd-user-instance-args args) args)))

(defprop daemon-reloaded :posix (&key user-instance)
  (:desc "Attempt to reload systemd manager configuration")
  (:apply (if (service:no-services-p)
              :no-change
              (systemctl #'mrun user-instance "daemon-reload"))))

(defprop started :posix (service &key user-instance)
  (:desc #?"systemd service ${service} started")
  (:check
   (or (service:no-services-p)
       (zerop (systemctl #'mrun user-instance :for-exit "is-active" service))))
  (:apply (systemctl #'mrun user-instance "start" service)))

(defprop stopped :posix (service &key user-instance)
  (:desc #?"systemd service ${service} stopped")
  (:check
   (or (service:no-services-p)
       (plusp (systemctl #'mrun user-instance :for-exit "is-active" service))))
  (:apply (systemctl #'mrun user-instance "stop" service)))

(defprop restarted :posix (service &key user-instance)
  (:desc #?"Attempt to restart systemd service ${service}")
  (:apply (if (service:no-services-p)
              :no-change
              (systemctl #'mrun user-instance "restart" service))))

(defprop reloaded :posix (service &key user-instance)
  (:desc #?"Attempt to reload systemd service ${service}")
  (:apply (if (service:no-services-p)
              :no-change
              (systemctl #'mrun user-instance "reload" service))))

(defprop enabled :posix (service &key user-instance)
  (:desc #?"systemd service ${service} enabled")
  (:check
   (or (and user-instance (service:no-services-p))
       (zerop
        (systemctl #'mrun user-instance :for-exit "is-enabled" service))))
  (:apply (systemctl #'mrun user-instance "enable" service)))

(defprop disabled :posix (service &key user-instance)
  (:desc #?"systemd service ${service} disabled")
  (:check
   (or
    (and user-instance (service:no-services-p))
    (let ((status
            (stripln
             (systemctl #'run user-instance :may-fail "is-enabled" service))))
      (or (string-prefix-p "linked" status)
          (string-prefix-p "masked" status)
          (memstr= status '("static" "disabled" "generated" "transient" "indirect"))))))
  (:apply (systemctl #'mrun user-instance "disable" service)))

(defprop masked :posix (service &key user-instance)
  (:desc #?"systemd service ${service} masked")
  (:check
   (or (and user-instance (service:no-services-p))
       (string-prefix-p
        "masked"
        (systemctl #'run user-instance :may-fail "is-enabled" service))))
  (:apply (systemctl #'mrun user-instance "mask" service))
  (:unapply (if (and user-instance (service:no-services-p))
                :no-change
                (systemctl #'mrun user-instance "unmask" service))))

(defprop lingering-enabled :posix (user)
  (:desc #?"User lingering enable for ${user}")
  (:check
   (or (service:no-services-p)
       ;; 'loginctl show-user' fails if the user is neither logged in nor
       ;; lingering.  There is no dedicated exit code for that, so just assume
       ;; lingering is not enabled if command exits nonzero.
       (multiple-value-bind (out err exit)
           (run :may-fail "loginctl" "show-user" user)
         (declare (ignore err))
         (and (zerop exit) (memstr= "Linger=yes" (lines out))))))
  (:apply (mrun "loginctl" "enable-linger" user))
  (:unapply (if (service:no-services-p)
                :no-change
                (mrun "loginctl" "disable-linger" user))))
