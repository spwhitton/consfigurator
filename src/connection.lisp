;;; Consfigurator -- Lisp declarative configuration management system

;;; Copyright (C) 2020-2021  Sean Whitton <spwhitton@spwhitton.name>

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

(in-package :consfigurator)

;;;; Connections

;; generic function operating on keywords which identify connection types
(defgeneric establish-connection (type remaining &key)
  (:documentation
   "Within the context of the current connection, connect to HOST by
establishing a new connection of type TYPE.
Either starts a Lisp process somewhere else, tells it to continue establishing
REMAINING (by telling it to call DEPLOY* with arguments obtained by (locally)
evaluating (list (or REMAINING '(:local)) *host*)), and returns nil, or
returns a object suitable to be the value of *CONNECTION*.

Any implementation which hands over to a remote Lisp process will need to
upload any prerequisite data required by the deployment."))

(defclass connection ()
  ((parent
    :initform *connection*
    :documentation
    "The value of *CONNECTION* at the time this connection was established.")))

(defclass lisp-connection (connection) ())

(defclass posix-connection (connection) ())

;;; generic functions to operate on subclasses of CONNECTION

(defgeneric connection-run (connection cmd &optional input)
  (:documentation "Subroutine to run shell commands on the host.

INPUT is a string to send to the shell command's stdin, or a stream which will
be emptied into the shell command's stdin.

Implementations can specialise on both the CONNECTION and INPUT arguments, if
they need to handle streams and strings differently.

Returns (values OUT EXIT) where OUT is merged stdout and stderr and EXIT is
the exit code.  Should not signal any error condition just because EXIT is
non-zero."))

(defmethod connection-run :around ((connection connection) cmd &optional input)
  (declare (ignore cmd input))
  (let ((*connection* (slot-value connection 'parent)))
    (call-next-method)))

(defgeneric connection-readfile (connection path)
  (:documentation "Subroutine to read the contents of files on the host."))

(defmethod connection-readfile :around ((connection connection) path)
  (declare (ignore path))
  (let ((*connection* (slot-value connection 'parent)))
    (call-next-method)))

;; only functional difference between WRITEFILE AND upload is what args they
;; take: a string vs. a path.  for a given connection type, they may have same
;; or different implementations.

(defgeneric connection-writefile (connection path input)
  (:documentation
   "Subroutine to replace/create the contents of files on the host.

INPUT is the new contents of the file or a stream which will produce it.

Implementations can specialise on both the CONNECTION and INPUT arguments, if
they need to handle streams and strings differently."))

(defmethod connection-writefile :around ((connection connection) path contents)
  (declare (ignore path contents))
  (let ((*connection* (slot-value connection 'parent)))
    (call-next-method)))

(defgeneric connection-upload (connection from to)
  (:documentation "Subroutine to upload files to the host.

Only used, internally, for uploading prerequisite data, and only to caches."))

(defmethod connection-upload :around ((connection connection) from to)
  (declare (ignore from to))
  (let ((*connection* (slot-value connection 'parent)))
    (call-next-method)))

(defgeneric connection-teardown (connection)
  (:documentation "Subroutine to disconnect from the host."))

(defmethod connection-teardown :around ((connection connection))
  (let ((*connection* (slot-value connection 'parent)))
    (call-next-method)))

;; many connection types don't need anything to be done to disconnect
(defmethod connection-teardown ((connection connection))
   (values))

;; global value gets set in connection/local.lisp, but the symbol is not
;; exported as it should only get bound by DEPLOY*
(defvar *connection* nil
  "Object representing the currently active connection.
Connections dynamically bind this variable and then apply properties.  Its
global value should be regarded as a constant.")


;;;; Functions to access the slots of the current connection

;; Used by properties and by implementations of ESTABLISH-CONNECTION.  This is
;; the only code that ever call CONNECTION-RUN, CONNECTION-READFILE and
;; CONNECTION-WRITEFILE directly (except that it might make sense for
;; implementations of CONNECTION-READFILE and CONNECTION-WRITEFILE to call
;; their corresponding implementation of CONNECTION-RUN).

(define-condition connection-run-failed (error)
  ((stdout :initarg stdout :reader stdout)
   (stderr :initarg stderr :reader stderr)
   (exit-code :initarg exit-code :reader exit-code)))

(defun run (&rest args)
  "Synchronous execution of shell commands using the current connection.
ARGS can contain keyword-value pairs (and singular keywords) to specify
aspects of this function's behaviour, and remaining elements of ARGS are the
shell command and its parameters, or, as a special case, a single string
specifying the shell command, with any necessary escaping already performed.
It is recommended that all keywords and corresponding values come first,
followed by argument(s) specifying the shell command to execute.

Keyword arguments accepted:

  - :for-exit / :may-fail -- don't signal an error condition if the command
    does not exit nonzero, usually because it is being called partly or only
    for its exit code

  - :input INPUT -- pass the contents of the string or stream INPUT on stdin

  - :env ENVIRONMENT -- where ENVIRONMENT is a plist specifying environment
    variable names and values, use env(1) to set these variables when running
    the command.

Returns command's stdout, stderr and exit code."
  (let (cmd input may-fail env (stderr (mktemp)))
    (loop for arg = (pop args)
	  do (case arg
	       (:for-exit (setq may-fail t))
	       (:may-fail (setq may-fail t))
	       (:input (setq input (pop args)))
	       (:env (setq env (pop args)))
	       (t (push arg cmd)))
	  while args
	  finally (nreversef cmd))
    (setq cmd (if (cdr cmd) (escape-sh-command cmd) (car cmd)))
    (loop while env
	  collect (format nil "~A=~A" (symbol-name (pop env)) (pop env))
	    into accum
	  finally
	     (when accum
	       (setq cmd (format nil "env ~A ~A"
				 (escape-sh-command accum)
				 cmd))))
    (unwind-protect
	 (multiple-value-bind (out exit)
	     (connection-run *connection*
			     (format nil "( ~A ) 2>~A" cmd stderr)
			     input)
	   (let ((err (readfile stderr)))
	     (if (or may-fail (= exit 0))
		 (values out err exit)
		 (error 'connection-run-failed
			:stdout out :stderr err :exit-code exit))))
      (connection-run *connection* (format nil "rm -f ~A" stderr)))))

(defun mktemp ()
  "Make a temporary file on the remote side."
  (multiple-value-bind (out exit)
      ;; mktemp(1) is not POSIX; the only POSIX way is this m4 way,
      ;; apparently, but even though m4(1) is POSIX it seems like it could
      ;; often be absent, so have a fallback.  Avoid passing any arguments to
      ;; mktemp(1) as these may differ on different platforms.
      (connection-run
       *connection*
       "echo 'mkstemp('${TMPDIR:-/tmp}'/tmp.XXXXXX)' | m4 2>/dev/null || mktemp")
    (if (= exit 0)
	(car (lines out))
	(error 'connection-run-failed :exit-code exit))))

(defun runlines (&rest args)
  (lines (apply #'run args)))

(defun readfile (&rest args)
  (apply #'connection-readfile *connection* args))

(defun writefile (&rest args)
  (apply #'connection-writefile *connection* args))