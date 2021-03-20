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

(in-package :consfigurator.property.service)
(named-readtables:in-readtable :interpol-syntax)

;;;; Controlling services using service(1)

(define-constant +policyrcd+ #P"/usr/sbin/policy-rc.d" :test #'equal)
(define-constant +policyrcd~+ #P"/usr/sbin/policy-rc.d~consfig" :test #'equal)

(defprop %no-services :posix ()
  (:hostattrs
   (push-hostattrs :no-services t)))

(defprop %policy-rc.d :posix ()
  (:apply
   (assert-euid-root)
   (when (test "-e" +policyrcd+ "-a" "!" "-e" +policyrcd~+)
     (mrun "mv" +policyrcd+ +policyrcd~+))
   (file:has-content +policyrcd+ '("#!/bin/sh" "exit 101"))
   (file:has-mode +policyrcd+ #o755))
  (:unapply
   (assert-euid-root)
   (if (test "-e" +policyrcd~+)
       (mrun "mv" +policyrcd~+ +policyrcd+)
       (file:does-not-exist +policyrcd+))))

(defproplist no-services :posix ()
  "Disable starting services with service(1) and by the package manager."
  (:desc #?"Starting services disabled")
  (%no-services)
  (os:typecase
    (debian (%policy-rc.d))))

(defprop running :posix (service)
  "Attempt to start service using service(1).
Assumes that if service(1) returns nonzero, it means the service was already
running.  If something more robust is required, use init system-specific
properties."
  (:desc #?"Attempt to start ${service} has been made")
  (:apply
   (unless (get-hostattrs-car :no-services)
     (run :may-fail "service" service "start"))))
