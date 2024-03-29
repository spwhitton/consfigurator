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
;;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

(in-package :consfigurator.property.service)
(named-readtables:in-readtable :consfigurator)

;;;; Controlling services using service(1), and the :NO-SERVICES hostattr and
;;;; connattr.  A host has the :NO-SERVICES hostattr when it has static
;;;; configuration never to start services.  The connattr is for when we
;;;; should not start any services because we're doing something like
;;;; chrooting in to the host's unbooted root filesystem.

(define-constant +policyrcd+ #P"/usr/sbin/policy-rc.d" :test #'equal)

(defprop %no-services :posix ()
  (:hostattrs
   (push-hostattr :no-services t)))

(defprop %policy-rc.d :posix ()
  (:apply
   (assert-remote-euid-root)
   (file:has-content +policyrcd+ '("#!/bin/sh" "exit 101"))
   (file:has-mode +policyrcd+ #o755))
  (:unapply
   (assert-remote-euid-root)
   (file:does-not-exist +policyrcd+)))

(defproplist no-services :posix ()
  "Disable starting services with service(1) and by the package manager.

The implementation for Debian and Debian derivatives is currently very
simplistic, and will interact badly with any other properties which want to
use /usr/sbin/policy-rc.d.  However, if for all other purposes you use systemd
configuration instead of editing /usr/sbin/policy-rc.d, this limitation should
not affect you."
  (:desc #?"Starting services disabled")
  (%no-services)
  (os:etypecase
      (debianlike (%policy-rc.d))))

(defun no-services-p ()
  "Returns true if no services should be started by the current deployment."
  (or (get-hostattrs-car :no-service) (get-connattr :no-services)))

(defun service (service action)
  (unless (no-services-p)
    (run :may-fail "service" service action)))

(defprop running :posix (service)
  "Attempt to start service using service(1).
Assumes that if service(1) returns nonzero, it means the service was already
running.  If something more robust is required, use init system-specific
properties."
  (:desc #?"Attempt to start ${service} has been made")
  (:apply
   (service service "start")
   ;; assume it was already running
   :no-change))

(defprop restarted :posix (service)
  (:desc #?"Attempt to restart ${service}")
  (:apply (service service "restart")))

(defprop reloaded :posix (service)
  (:desc #?"Attempt to reload ${service}")
  (:apply (service service "reload")))

(define-function-property-combinator without-starting-services (&rest propapps)
  "Apply PROPAPPS with the :NO-SERVICES connattr temporarily in effect.  Also
disable starting services by the package manager."
  (let ((propapp (if (cdr propapps) (apply #'eseqprops propapps) (car propapps))))
    (:retprop :type :lisp
              :hostattrs (lambda ()
                           (propapp-attrs propapp)
                           (os:required 'os:debianlike))
              :apply
              (lambda (&aux (already-exists (file-exists-p +policyrcd+)))
                (with-remote-temporary-file (temp :directory "/usr/sbin")
                  (when already-exists
                    (rename-file +policyrcd+ temp))
                  (%policy-rc.d)
                  (let ((before (get-universal-time)))
                    ;; Sleep for one second so that we know BEFORE is in the
                    ;; past.  (SLEEP 1) is only approximately one second so
                    ;; check that it's actually been a second.
                    (loop do (sleep 1) until (> (get-universal-time) before))
                    (unwind-protect (with-connattrs (:no-services t)
                                      (apply-propapp propapp))
                      (if already-exists
                          ;; Check whether some property we applied set the
                          ;; contents of /usr/sbin/policy-rc.d, in which case
                          ;; we won't restore our backup.
                          (unless (> (file-write-date +policyrcd+) before)
                            (rename-file temp +policyrcd+))
                          (when (file-exists-p +policyrcd+)
                            (delete-file +policyrcd+)))))))
              :unapply (lambda () (unapply-propapp propapp)))))
