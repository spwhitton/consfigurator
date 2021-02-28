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

(in-package :consfigurator.connection.debian-sbcl)

(defmethod establish-connection ((type (eql :debian-sbcl)) remaining &key)
  (mrun "which sbcl >/dev/null 2>&1 || apt-get -y install sbcl")
  (request-lisp-systems)
  (upload-all-prerequisite-data)
  (princ "Waiting for remote Lisp to exit, this may take some time ... ")
  (force-output)
  (let ((program (continue-deploy*-program remaining)))
    (multiple-value-bind (out err exit)
	(run :may-fail :input program
	     "sbcl" "--noinform" "--noprint"
	     "--disable-debugger"
	     "--no-sysinit" "--no-user-init")
      (format t "done.")
      (if (zerop exit)
	  (format t "  Output was:~%~{    ~A~%~}" (lines out))
	  (error "~%~%Remote Lisp failed; we sent~%~%~A~%~%and stderr was:~%~A"
		 program err))))
  nil)
