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

(defproplist sbcl-available :posix ()
  (os:etypecase
    (debianlike (apt:installed "sbcl"))))

(defparameter *sbcl* '("sbcl" "--noinform" "--noprint"
                       "--disable-debugger" "--no-sysinit" "--no-userinit"))

(defmethod establish-connection ((type (eql :sbcl)) remaining &key)
  (when (lisp-connection-p)
    (warn
     "Looks like you might be starting a fresh Lisp image directly from the root
Lisp. This can mean that prerequisite data gets extracted from encrypted
stores and stored unencrypted under ~~/.cache, and as such is not
recommended."))
  (unless (zerop (mrun :for-exit "command" "-v" "sbcl"))
    ;; If we're not the final hop then we don't know the OS of the host to
    ;; which we're currently connected, so we can't apply SBCL-AVAILABLE.
    ;;
    ;; TODO In the case of INSTALLER:CLEANLY-INSTALLED-ONCE this code will
    ;; have us trying to use apt to install sbcl on a Fedora host, say, upon
    ;; the first connection, before Debian has been installed.  Perhaps we
    ;; should just have some code which tries to install sbcl based on the
    ;; package manager(s) it can find on PATH.  Could reuse that code for
    ;; CHROOT::%DEBOOTSTRAP-MANUALLY-INSTALLED.
    (if remaining
        (failed-change "sbcl not on PATH and don't know how to install.")
        (sbcl-available)))
  (let ((requirements (asdf-requirements-for-host-and-features
                       (safe-read-from-string
                        (run :input "(prin1 *features*)" *sbcl*)
                        :package :cl-user))))
    (request-asdf-requirements requirements)
    (upload-all-prerequisite-data)
    (inform t "Waiting for remote Lisp to exit, this may take some time ... ")
    (force-output)
    (multiple-value-bind (program forms)
        (continue-deploy*-program remaining requirements)
      (multiple-value-bind (out err exit) (run :may-fail :input program *sbcl*)
        (inform t (if (< exit 3) "done." "failed.") :fresh-line nil)
        (when-let ((lines (lines out)))
          (inform t "  Output was:" :fresh-line nil)
          (with-indented-inform (inform t lines)))
        (return-exit
         exit
         ;; print FORMS not PROGRAM because latter might contain sudo passwords
         :on-failure
         (failed-change
	  "~&Remote Lisp failed; stderr was:~%~%~A~&~%Program we sent:~%~%~S"
          err forms))))))
