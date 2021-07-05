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

(in-package :consfigurator.connection.fork)
(named-readtables:in-readtable :consfigurator)

;; Use only implementation-specific fork and waitpid calls to avoid thread
;; woes.  Things like chroot(2) and setuid(2), however, should be okay.

(defun fork ()
  #+sbcl (sb-posix:fork))

(defun waitpid (pid options)
  ;; normalise any other implementations such that we always return
  ;; (values PID EXIT-STATUS), as SB-POSIX:WAITPID does
  #+sbcl (sb-posix:waitpid pid options))

(defun wifexited (status)
  #+sbcl (sb-posix:wifexited status))

(defun wexitstatus (status)
  #+sbcl (sb-posix:wexitstatus status))

(defun can-probably-fork ()
  "Return nil if we can detect other running threads, and the Lisp
implementation is known not to support forking when there are other threads.
A return value other than nil indicates only that we couldn't detect
circumstances in which it is known that we cannot fork, not that we are sure
we can fork -- a thread might be only partly initialised at the time we check,
for example, such that we don't see it."
  (and
   #+sbcl (> 2 (length (sb-thread:list-all-threads)))))

(defclass fork-connection (local-connection) ())

(defgeneric post-fork (connection)
  (:documentation
   "Code to execute after forking but before calling CONTINUE-DEPLOY*."))

(defmethod continue-connection ((connection fork-connection) remaining)
  (unless (lisp-connection-p)
    (error "Forking requires a Lisp-type connection."))
  #-(or sbcl) (error "Don't know how to safely fork() in this Lisp")
  (upload-all-prerequisite-data
   :connection connection :upload-string-data nil)
  (with-remote-temporary-file (output)
    (mapc #'force-output
          (list *standard-output* *error-output* *debug-io* *terminal-io*))
    (let ((child (fork)))
      (case child
        ;; note that SB-POSIX:FORK can only return >=0
        (-1
         (error "fork(2) failed"))
        (0
         (with-backtrace-and-exit-code
           ;; Capture child stdout in case *STANDARD-OUTPUT* has been rebound
           ;; to somewhere else in the parent, e.g. by APPLY-AND-PRINT.  The
           ;; parent can then send the contents of the file named by OUTPUT to
           ;; the correct stream.  We don't use pipe(2) because then we'd need
           ;; implementation-specific code to bind streams to the FDs.
           (with-open-file (*standard-output*
                            output :direction :output :if-exists :append)
             (mapc #'clear-input
                   (list *standard-input* *debug-io* *terminal-io*))
             (cancel-unwind-protect-in-parent-cleanup)
             ;; While some kinds of data source will still work given certain
             ;; subtypes of FORK-CONNECTION (e.g. if they've already cached
             ;; the data in memory, or if it's also accessible to whomever we
             ;; will SETUID to), others won't, so drop all registrations and
             ;; rely on the call to UPLOAD-ALL-PREREQUISITE-DATA above.
             (reset-data-sources)
             (post-fork connection)
             ;; It would be nice to reenter Consfigurator's primary loop by
             ;; just calling (return-from establish-connection
             ;; (establish-connection :local)) here, but we need to kill off
             ;; the child afterwards, rather than returning to the child's
             ;; REPL or whatever else.
             (continue-deploy* connection remaining))))
        (t
         (multiple-value-bind (pid status) (waitpid child 0)
           (declare (ignore pid))
           (fresh-line)
           (princ (readfile output))
           (let ((exited (wifexited status)))
             (unless exited
               (failed-change
                "Fork connection child did not exit normally, status #x~(~4,'0X~)"
                status))
             (let ((exit-status (wexitstatus status)))
               (return-exit
                exit-status
                :on-failure
                (failed-change "Fork connection child failed, exit code ~D"
                               exit-status))))))))))
