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

(in-package :consfigurator.connection.sbcl)
(named-readtables:in-readtable :consfigurator)

(defparameter *sbcl* '("sbcl" "--noinform" "--noprint"
                       "--disable-debugger" "--no-sysinit" "--no-userinit"))

(defmethod establish-connection
    ((type (eql :sbcl)) remaining
     &key (package-manager nil package-manager-supplied-p))
  "Start up a remote Lisp image using SBCL.

Specifying PACKAGE-MANAGER avoids the need to see what package managers are
available on PATH, which can provide a performance improvement."
  (when (lisp-connection-p)
    (warn
     "Looks like you might be starting a fresh Lisp image directly from the root
Lisp. This can mean that prerequisite data gets extracted from encrypted
stores and stored unencrypted under ~~/.cache, and as such is not
recommended."))
  ;; Allow the user to request no attempt to install the dependencies at all,
  ;; perhaps because they know they're already manually installed.
  (unless (and package-manager-supplied-p (not package-manager))
    (handler-case (package:installed
                   package-manager '(:apt "sbcl")
                   package:+consfigurator-system-dependencies+)
      ;; If we couldn't find any package manager on PATH, just proceed in the
      ;; hope that everything we need is already installed; we'll find out
      ;; whether it's actually a problem pretty quickly, when the remote SBCL
      ;; tries to compile and load the ASDF systems.
      (package:package-manager-not-found (c)
        (apply #'warn (simple-condition-format-control c)
               (simple-condition-format-arguments c)))))
  (let ((requirements (asdf-requirements-for-host-and-features
                       (safe-read-from-string
                        (run :input "(prin1 *features*)" *sbcl*)
                        :package :cl-user))))
    ;; Don't preserve the ASDF requirements :DATA hostattrs because they are
    ;; valid only for this hop, not necessarily beyond here.  For example, if
    ;; we have a connection chain like (:ssh :sbcl (:lxc :name ...)) then we
    ;; don't want to upload all the ASDF systems into the container.
    (with-preserve-hostattrs
      (request-asdf-requirements requirements) (upload-all-prerequisite-data))
    (inform t "Waiting for remote Lisp to exit, this may take some time ... ")
    (force-output)
    (multiple-value-bind (program forms)
        (continue-deploy*-program remaining requirements)
      (multiple-value-bind (out err exit) (run :may-fail :input program *sbcl*)
        (inform t (if (member exit '(0 22 23)) "done." "failed.") :fresh-line nil)
        (when-let ((lines (lines out)))
          (inform t "  Output was:" :fresh-line nil)
          (with-indented-inform (inform t lines)))
        (exit-code-to-retval
         exit
         ;; print FORMS not PROGRAM because latter might contain sudo passwords
         :on-failure
         (failed-change
	  "~&Remote Lisp failed; stderr was:~%~%~A~&~%Program we sent:~%~%~S"
          err forms))))))
