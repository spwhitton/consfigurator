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
  (:apply (mrun "systemctl" "start" service)
   :no-change))

(defprop stopped :posix (service)
  (:desc #?"systemd service ${service} stopped")
  (:apply (mrun "systemctl" "stop" service)
   :no-change))

(defprop enabled :posix (service)
  (:desc #?"systemd service ${service} enabled")
  (:apply (mrun "systemctl" "enable" service)
   :no-change))

(defprop disabled :posix (service)
  (:desc #?"systemd service ${service} disabled")
  (:apply (mrun "systemctl" "disable" service)
   :no-change))

(defprop masked :posix (service)
  (:desc #?"systemd service ${service} masked")
  (:apply (mrun "systemctl" "mask" service)
   :no-change)
  (:unapply (mrun "systemctl" "unmask" service)
   :no-change))
