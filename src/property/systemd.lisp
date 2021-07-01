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

(defprop started :posix (service)
  (:desc #?"systemd service ${service} started")
  (:check (zerop (mrun :for-exit "systemctl" "is-active" service)))
  (:apply (mrun "systemctl" "start" service)))

(defprop stopped :posix (service)
  (:desc #?"systemd service ${service} stopped")
  (:check (plusp (mrun :for-exit "systemctl" "is-active" service)))
  (:apply (mrun "systemctl" "stop" service)))

(defprop enabled :posix (service)
  (:desc #?"systemd service ${service} enabled")
  (:check (zerop (mrun :for-exit "systemctl" "is-enabled" service)))
  (:apply (mrun "systemctl" "enable" service)))

(defprop disabled :posix (service)
  (:desc #?"systemd service ${service} disabled")
  (:check
   (let ((status (stripln (run :may-fail "systemctl" "is-enabled" service))))
     (or (string-prefix-p "linked" status)
         (string-prefix-p "masked" status)
         (memstring=
          status
          '("static" "disabled" "generated" "transient" "indirect")))))
  (:apply (mrun "systemctl" "disable" service)))

(defprop masked :posix (service)
  (:desc #?"systemd service ${service} masked")
  (:check (string-prefix-p "masked"
                           (run :may-fail "systemctl" "is-enabled" service)))
  (:apply (mrun "systemctl" "mask" service))
  (:unapply (mrun "systemctl" "unmask" service)))
