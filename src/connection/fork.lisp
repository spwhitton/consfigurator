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
#+sbcl (eval-when (:compile-toplevel :load-toplevel :execute)
         (require "sb-posix"))

;; Use only implementation-specific fork and waitpid calls to avoid thread
;; woes.  Things like chroot(2) and setuid(2), however, should be okay.

(defun fork ()
  #+sbcl (sb-posix:fork))

(defun waitpid (pid options)
  ;; normalise any other implementations such that we always return
  ;; (values PID EXIT-STATUS), as SB-POSIX:WAITPID does
  #+sbcl (sb-posix:waitpid pid options))

(defmacro with-fork-connection ((remaining) &body forms)
  `(progn
     (unless (lisp-connection-p)
       (error "Forking requires a Lisp-type connection."))
     #-(or sbcl) (error "Don't know how to safely fork() in this Lisp")
     ;; TODO copy required prerequisite data into the chroot -- propellor uses
     ;; a bind mount but we might be the root Lisp, in which case we don't
     ;; have a cache to bind mount in.  use chroot.shell connection to upload?
     (mapc #'force-output
           (list *standard-output* *error-output* *debug-io* *terminal-io*))
     (let ((child (fork)))
       (case child
         ;; note that SB-POSIX:FORK can only return >=0
         (-1
          (error "fork(2) failed"))
         (0
          (handler-case
              (progn
                ;; TODO either (reset-data-sources), or bind a restart to
                ;; convert data source errors into failed-change (or ignore
                ;; them?  or what?), as they may or may not be available
                ;; inside the chroot, depending on whether the data source
                ;; code needs to read files outside of the chroot or already
                ;; has the data cached, a socket open etc.
                (mapc #'clear-input
                      (list *standard-input* *debug-io* *terminal-io*))
                ,@forms
                ;; it would be nice to reenter Consfigurator's primary loop by
                ;; just calling (return-from establish-connection
                ;; (establish-connection :local)) here, but we need to kill
                ;; off the child afterwards, rather than returning to the
                ;; child's REPL or whatever else
                (continue-deploy* ,remaining)
                (uiop:quit 0))
            (serious-condition (c)
              (format *error-output* "Fork connection child failed: ~A~%" c)
              (uiop:quit 2))))
         (t
          (multiple-value-bind (_ status) (waitpid child 0)
            (declare (ignore _))
            (unless (zerop status)
              ;; TODO instead of parsing the status ourselves here, maybe we
              ;; can call the various C macros for parsing the status in
              ;; wait(2)
              (error
               "Fork connection child failed, status #x~(~4,'0X~)" status)))
          nil)))))
