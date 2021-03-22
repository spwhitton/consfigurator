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

(in-package :consfigurator.connection.sbcl)
(named-readtables:in-readtable :consfigurator)

(defprop sbcl-available :posix ()
  (:check
   (zerop (mrun :for-exit "command" "-v" "sbcl")))
  (:apply
   (typecase (class-of (get-hostattrs-car :os))
     (os:debianlike
      (ignoring-hostattrs (apt:installed "sbcl")))
     (t
      (failed-change "Do not know how to install SBCL on this OS.")))))

(defmethod establish-connection ((type (eql :sbcl)) remaining &key)
  (sbcl-available)
  (request-lisp-systems)
  (upload-all-prerequisite-data)
  (inform t "Waiting for remote Lisp to exit, this may take some time ... ")
  (force-output)
  (multiple-value-bind (program forms)
      (continue-deploy*-program remaining)
    (multiple-value-bind (out err exit)
        (run :may-fail :input program
             "sbcl" "--noinform" "--noprint"
             "--disable-debugger"
             "--no-sysinit" "--no-user-init")
      (inform t "done." :fresh-line nil)
      (unless (zerop exit)
        ;; print FORMS not PROGRAM because latter might contain sudo passwords
        (error "~%~%Remote Lisp failed; we sent~%~%~A~%~%and stderr was:~%~A"
               forms err))
      (inform t "  Output was:" :fresh-line nil)
      (with-indented-inform (inform t (lines out)))))
  nil)
