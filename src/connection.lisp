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
(named-readtables:in-readtable :consfigurator)

;;;; Connections

(defvar *connection* nil
  "Object representing the currently active connection.
Deployments dynamically bind this variable and then apply properties.")

(defvar *host* nil
  "Object representing the host at the end of the current connection chain.
Deployments bind this variable.  Its global value should remain nil.

The main point of this is to allow properties to access the context in which
they're being applied.")

;; generic function operating on keywords which identify connection types
(defgeneric establish-connection (type remaining &key)
  (:documentation
   "Within the context of the current connection, connect to HOST by
establishing a new connection of type TYPE.
Either returns an object suitable to be the value of *CONNECTION*, or calls
either CONTINUE-DEPLOY* or CONTINUE-DEPLOY*-PROGRAM and returns nil.

Any implementation which calls CONTINUE-DEPLOY*-PROGRAM will need to call
UPLOAD-ALL-PREREQUISITE-DATA."))

(defgeneric continue-connection (connection remaining)
  (:documentation
   "Called by implementations of ESTABLISH-CONNECTION which return nil.
Calls CONTINUE-DEPLOY* or CONTINUE-DEPLOY*-PROGRAM."))

(defgeneric preprocess-connection-args (type &key)
  (:documentation
   "Hook to allow connection types to do work in the root Lisp before
Consfigurator begins the attempt to establish the connection chain.  The
return value is used as replacement keyword arguments to the connection.

For an example of usage, see the :SUDO connection type."))

(defmethod preprocess-connection-args ((type symbol) &rest args &key &allow-other-keys)
  (cons type args))

(defclass connection ()
  ((parent
    :initform *connection*
    :reader connection-parent
    :documentation
    "The value of *CONNECTION* at the time this connection was established.")
   (cached-data
    :initform nil
    :documentation
    "Items of prerequisite data known to be cached on the remote side.")
   (remote-home
    :initform nil
    :documentation
    "The remote user's home directory.")
   (current-directory
    :initform nil
    :documentation
    "The current working directory for RUN, MRUN, READFILE and WRITEFILE.")
   (remote-uid
    :initform nil
    :documentation
    "Effective user-id of the remote (deploying) user")
   (remote-user
    :initform nil
    :documentation
    "The name of the remote user.")))

(defun reset-remote-home ()
  "Clear the cache of the remote user's home directory, uid and name.
Used by implementations of ESTABLISH-CONNECTION which will invalidate the
cached values, such as a connection which forks and then SETUIDs to another
user.  Should not be called by properties."
  (setf (slot-value *connection* 'remote-home) nil
        (slot-value *connection* 'remote-uid) nil
        (slot-value *connection* 'remote-user) nil))

(defclass lisp-connection (connection) ())

(defclass posix-connection (connection) ())

(defun lisp-connection-p ()
  (subtypep (type-of *connection*) 'lisp-connection))

;;; generic functions to operate on subclasses of CONNECTION

(defgeneric connection-run (connection cmd input)
  (:documentation "Subroutine to run shell commands on the host.

INPUT is a string to send to the shell command's stdin, or a stream which will
be emptied into the shell command's stdin.

Implementations can specialise on both the CONNECTION and INPUT arguments, if
they need to handle streams and strings differently.

Returns (values OUT EXIT) where OUT is either merged stdout and stderr or
stderr followed by stdout, and EXIT is the exit code.  Should not signal any
error condition just because EXIT is non-zero."))

(defmethod connection-run :around ((connection connection) cmd input)
  (declare (ignore cmd input))
  (let ((*connection* (slot-value connection 'parent)))
    (call-next-method)))

(defgeneric connection-readfile (connection path)
  (:documentation "Subroutine to read the contents of files on the host."))

(defmethod connection-readfile :around ((connection connection) path)
  (declare (ignore path))
  (let ((*connection* (slot-value connection 'parent)))
    (call-next-method)))

;; only functional difference between WRITEFILE and UPLOAD is what args they
;; take: a string vs. a path.  for a given connection type, they may have same
;; or different implementations.

(defgeneric connection-writefile (connection path content mode)
  (:documentation
   "Subroutine to replace/create the contents of files on the host.

CONTENT is the new contents of the file or a stream which will produce it.

MODE is the numeric mode that the file should have by the time this function
returns.  Implementations should ensure that CONTENT is not stored on disk
with a mode greater than MODE, and also that if CONTENT is stored on disk
outside of (UIOP:PATHNAME-DIRECTORY-PATHNAME PATH), then it does not
have a mode greater than 700.  It is recommended that implementations write
CONTENT to a temporary file in (UIOP:PATHNAME-DIRECTORY-PATHNAME PATH),
change the mode of that file to MODE, and then rename to PATH.
WITH-REMOTE-TEMPORARY-FILE can be used to do this.

Implementations can specialise on both the CONNECTION and CONTENT arguments,
if they need to handle streams and strings differently."))

(defmethod connection-writefile :around ((connection connection)
                                         path
                                         content
                                         mode)
  (declare (ignore path content mode))
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


;;;; Functions to access the slots of the current connection

;; Used by properties and by implementations of ESTABLISH-CONNECTION.  This is
;; the only code that ever call CONNECTION-RUN, CONNECTION-READFILE and
;; CONNECTION-WRITEFILE directly (except that it might make sense for
;; implementations of CONNECTION-READFILE and CONNECTION-WRITEFILE to call
;; their corresponding implementation of CONNECTION-RUN).

(define-condition run-failed (error)
  ((cmd :initarg :cmd :reader failed-cmd)
   (stdout :initarg :stdout :reader failed-stdout)
   (stderr :initarg :stderr :reader failed-stderr)
   (exit-code :initarg :exit-code :reader failed-exit-code))
  (:report (lambda (condition stream)
             (format
              stream
              "~&'~A' failed, exit code ~A~%~%stdout was:~%~A~&~%stderr:~%~A"
              (failed-cmd condition)
              (failed-exit-code condition)
              (failed-stdout condition)
              (failed-stderr condition)))))

(defmacro with-remote-temporary-file ((file
                                       &key
                                         (connection '*connection*)
                                         (directory nil directory-supplied-p))
                                      &body body)
  "Execute BODY with FILE containing the path to a freshly created remote file,
which will be cleaned up when BODY is finished."
  ;; it would be nicer if we could just use (file &rest args) but we need to
  ;; look at CONNECTION ourselves, and we need to avoid CONNECTION being
  ;; evaluated more than once
  (once-only (connection)
    `(let ((,file (mktemp ,@(and directory-supplied-p
                                 `(:directory ,directory))
                          :connection ,connection)))
       (unwind-protect
            (progn ,@body)
         (connection-run ,connection
                         (format nil "rm -f ~A" (escape-sh-token ,file))
                         nil)))))

(defun mktemp (&key (connection *connection*) directory)
  "Make a temporary file on the remote side, in DIRECTORY, defaulting to /tmp."
  (let ((template (if directory
                      (unix-namestring
                       (merge-pathnames
                        "tmp.XXXXXX" (ensure-directory-pathname directory)))
                      "'${TMPDIR:-/tmp}'/tmp.XXXXXX")))
    (multiple-value-bind (out exit)
        ;; mktemp(1) is not POSIX; the only POSIX way is this M4 way,
        ;; apparently, but even though m4(1) is POSIX it seems like it could
        ;; often be absent, so have a fallback.  It would be better to avoid
        ;; passing any arguments to mktemp(1) as these may differ on different
        ;; platforms, but hopefully just a template is okay.
        ;;
        ;; While GNU M4 mkstemp makes the temporary file at most readable and
        ;; writeable by its owner, POSIX doesn't require this, so set a umask.
        (connection-run
         connection
         #?"umask 077; echo 'mkstemp(${template})' | m4 2>/dev/null || mktemp '${template}'"
         nil)
      (let ((lines (lines out)))
        (if (and (zerop exit) lines)
            (car lines)
            (error 'run-failed
                   :cmd "(attempt to make a temporary file on remote)"
                   :stdout out
                   :stderr "(merged with stdout)"
                   :exit-code exit))))))

(defmacro with-remote-current-directory ((dir) &body forms)
  "Execute FORMS with the current working directory DIR.
This affects the working directory for commands run using RUN and MRUN, and
the resolution of relative pathnames passed as the first argument of READFILE
and WRITEFILE.  For Lisp-type connections, it additionally temporarily sets
the working directory of the Lisp process using UIOP:WITH-CURRENT-DIRECTORY."
  (with-gensyms (previous new)
    `(let ((,previous (slot-value *connection* 'current-directory))
           (,new (ensure-pathname ,dir
                                  :defaults (pwd)
                                  :ensure-absolute t :ensure-directory t)))
       (setf (slot-value *connection* 'current-directory) ,new)
       (unwind-protect
            (if (lisp-connection-p)
                (with-current-directory (,new) ,@forms)
                (progn ,@forms))
         (setf (slot-value *connection* 'current-directory) ,previous)))))

(defun pwd ()
  (or (slot-value *connection* 'current-directory)
      (slot-value *connection* 'remote-home)
      (setf (slot-value *connection* 'remote-home)
            (let ((home
                    (stripln (connection-run *connection* "echo $HOME" nil))))
              (if (string-equal "" home)
                  (error "Failed to determine remote home directory.")
                  (ensure-directory-pathname home))))))

(defmacro %process-run-args (&body forms)
  `(let (cmd input may-fail for-exit env inform)
     (loop for arg = (pop args)
           do (case arg
                (:for-exit  (setq may-fail t for-exit t))
                (:may-fail  (setq may-fail t))
                (:inform    (setq inform t))
                (:input (setq input (pop args)))
                (:env (setq env (pop args)))
                (t (mapc (lambda (e)
                           (push (typecase e
                                   (pathname
                                    (unix-namestring e))
                                   (t
                                    e))
                                 cmd))
                         (ensure-list arg))))
           while args
           finally (nreversef cmd))
     (setq cmd (if (cdr cmd) (escape-sh-command cmd) (car cmd)))
     (loop while env
           collect (format nil "~A=~A"
                           (symbol-name (pop env)) (escape-sh-token (pop env)))
             into accum
           finally
              (when accum
                ;; We take this approach of exporting individual variables
                ;; rather than just prepending `FOO=bar BAR=baz` so that if CMD
                ;; contains $FOO it will get expanded.  We used to use env(1)
                ;; but that means CMD cannot contain shell builtins which do
                ;; not have an equivalent on PATH, such as 'cd'.  This approach
                ;; does mean that implementations of CONNECTION-RUN will need
                ;; to start a fresh 'sh -c' for each command run, but that's
                ;; desirable to ensure any variables set by CMD are reset.
                (setq cmd (format nil "~{export ~A;~^ ~} ~A" accum cmd))))
     (setq cmd (format nil "cd ~A; ~A"
                       (escape-sh-token (unix-namestring (pwd))) cmd))
     ,@forms))

(defun run (&rest args)
  "Synchronous execution of shell commands using the current connection.
ARGS can contain keyword-value pairs (and singular keywords) to specify
aspects of this function's behaviour, and remaining elements of ARGS are the
shell command and its parameters, or, as a special case, a single string
specifying the shell command, with any necessary escaping already performed.
It is recommended that all keywords and corresponding values come first,
followed by argument(s) specifying the shell command to execute.

You can additionally supply lists of arguments and these will be spliced into
the resulting list of arguments to be passed to the command.  I.e.
(run \"a\" (list \"b\" \"c\")) is equivalent to (run \"a\" \"b\" \"c\").

Keyword arguments accepted:

  - :FOR-EXIT / :MAY-FAIL -- don't signal an error condition if the command
    does not exit nonzero, usually because it is being called partly or only
    for its exit code

  - :INFORM -- send a copy of the output to *STANDARD-OUTPUT*

  - :INPUT INPUT -- pass the content of the string or stream INPUT on stdin

  - :ENV ENVIRONMENT -- where ENVIRONMENT is a plist specifying environment
    variable names and values, use env(1) to set these variables when running
    the command.

Returns command's stdout, stderr and exit code, unless :FOR-EXIT, in which
case return only the exit code."
  (%process-run-args
    (with-remote-temporary-file (stdout)
      (setq cmd (format nil "( ~A ) >~A" cmd stdout))
      (informat 3 "~&RUN ~A" cmd)
      (multiple-value-bind (err exit)
          (connection-run *connection* cmd input)
        (let ((out (readfile stdout)))
          (when inform (informat 1 "~&    % ~A~%~{    ~A~%~}" cmd (lines out)))
          (if (or may-fail (= exit 0))
              (if for-exit exit (values out err exit))
              (error 'run-failed
                     :cmd cmd :stdout out :stderr err :exit-code exit)))))))

(defun mrun (&rest args)
  "Like RUN but don't separate stdout and stderr (\"m\" for \"merged\"; note
that this might mean interleaved or simply concatenated, depending on the
connection chain).

Some (but not all) connection types will want to use this when implementing
ESTABLISH-CONNECTION, CONNECTION-RUN, CONNECTION-WRITEFILE etc. to avoid the
overhead of splitting the output streams only to immediately recombine them.

Some :POSIX properties which want to run a lot of commands and don't need to
separate the streams might want to use this too, but usually it is best to
start with RUN."
  (%process-run-args
    (informat 3 "~&MRUN ~A" cmd)
    (multiple-value-bind (out exit)
        (connection-run *connection* cmd input)
      (when inform (informat 1 "~&    % ~A~%~{    ~A~%~}" cmd (lines out)))
      (if (or may-fail (= exit 0))
          (if for-exit exit (values out exit))
          (error 'run-failed
                 :cmd cmd
                 :stdout out
                 :stderr "(merged with stdout)"
                 :exit-code exit)))))

(defun runlines (&rest args)
  (lines (apply #'run args)))

(defun test (&rest args)
  (zerop (apply #'mrun :for-exit "test" args)))

(defun delete-remote-trees (&rest paths)
  "Recursively delete each of PATHS."
  (mrun "rm" "-rf" paths))

(defun remote-exists-p (&rest paths)
  "Does each of PATHS exists?
PATH may be any kind of file, including directories."
  (apply #'test (loop for path on paths
                      nconc (list "-e" (car path))
                      when (cdr path) collect "-a")))

(defun readfile (path)
  (connection-readfile
   *connection*
   (unix-namestring
    (ensure-pathname path
                     :namestring :unix
                     :defaults (pwd)
                     :ensure-absolute t))))

(defun writefile (path content
                  &key (mode #o644 mode-supplied-p)
                  &aux (pathname (ensure-pathname path
                                                  :namestring :unix
                                                  :defaults (pwd)
                                                  :ensure-absolute t))
                    (namestring (unix-namestring pathname)))
  ;; If (lisp-connection-p), the file already exists, and it's not owned by
  ;; us, we could (have a keyword argument to) bypass CONNECTION-WRITEFILE and
  ;; just WRITE-STRING to the file.  That way we don't replace the file with
  ;; one owned by us, which we might not be able to chown back as non-root.
  ;;
  ;; The following, simpler behaviour should fit most sysadmin needs.
  (if (remote-exists-p pathname)
      ;; seems there is nothing like stat(1) in POSIX, and note that
      ;; --reference for chmod(1) and chown(1) is not POSIX
      (flet ((dehyphen (s) (delete #\- s)))
        (multiple-value-bind (match groups)
            (re:scan-to-strings #?/^.(...)(...)(...).[0-9]+ ([0-9]+) ([0-9]+) /
                                (run "ls" "-nd" pathname))
          (unless match
            (error
             "WRITEFILE could not determine ownership and mode of ~A" pathname))
          (let ((umode (dehyphen (elt groups 0)))
                (gmode (dehyphen (elt groups 1)))
                (omode (dehyphen (elt groups 2)))
                (uid (elt groups 3))
                (gid (elt groups 4)))
            (connection-writefile *connection* namestring content mode)
            (let ((namestring (escape-sh-token namestring)))
              (unless mode-supplied-p
                ;; assume that if we can write it we can chmod it
                (mrun #?"chmod u=${umode},g=${gmode},o=${omode} ${namestring}"))
              ;; we may not be able to chown; that's okay
              (mrun :may-fail #?"chown ${uid}:${gid} ${namestring}")))))
      (connection-writefile *connection* namestring content mode)))

(defmacro with-local-connection (&body forms)
  "Execute FORMS as though a :LOCAL connection had been established.
This is for testing purposes at the REPL and in Consfigurator's test suite.
FORMS is typically the programmatic application of one or more properties."
  `(let ((*host* (make-host :propspec (make-propspec :systems nil)))
         (*connection* (establish-connection :local nil)))
     ,@forms))

(defmacro with-local-passwordless-sudo-connection (&body forms)
  "Execute FORMS as though a passwordless :SUDO connection to localhost had been
established.  Occasionally useful for testing purposes at the REPL."
  `(with-local-connection
     (let ((*connection* (establish-connection :sudo nil
                                               :user "root" :password nil)))
       ,@forms)))
