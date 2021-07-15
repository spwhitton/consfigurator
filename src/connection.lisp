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
   (connattrs
    :initarg :connattrs
    :initform nil
    :documentation "This connection's connection attributes.")))

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

(defgeneric connection-connattr (connection k)
  (:documentation "Get the connattr identified by K for CONNECTION.")
  (:method :around ((connection connection) (k symbol))
    "Retrieve stored connattr or call next method to determine connattr."
    (or (getf (slot-value connection 'connattrs) k)
        (setf (getf (slot-value connection 'connattrs) k)
              (let ((*connection* (slot-value connection 'parent)))
                (call-next-method)))))
    (:method ((connection connection) (k symbol))
    "Default: if no stored value, there is no connattr identified by K."
    nil))

(defun (setf connection-connattr) (v connection k)
  (setf (getf (slot-value connection 'connattrs) k) v))

(defgeneric propagate-connattr (type connattr connection)
  (:documentation
   "Possibly propagate CONNATTR, a connattr identified by TYPE, through to the
newly-established CONNECTION.  Implementations should specialise on TYPE and
CONNECTION, not modify any of their arguments, and either return the new
connattr, or nil if nothing should be propagated.")
  (:method (type connattr connection)
    "Default implementation: don't propagate."
    nil))

(defmethod initialize-instance :after ((connection connection) &key)
  "Propagate connattrs which should be propagated."
  (with-slots (parent) connection
    (when (and parent (slot-boundp parent 'connattrs))
      (doplist (k v (slot-value parent 'connattrs))
               (when-let ((new (propagate-connattr k v connection)))
                 (setf (connection-connattr connection k) new))))))


;;;; Default methods to set some global connattrs

;;; For connection types where this default implementations won't work, either
;;; set the value of the connattr in ESTABLISH-CONNECTION or provide an
;;; implementation specialising on both arguments.

(defmethod connection-connattr ((connection connection) (k (eql 'id)))
  (multiple-value-bind (out exit) (connection-run connection "id" nil)
    (if (zerop exit)
        (stripln out)
        (failed-change "Failed to run id(1) on remote system."))))

(defmethod connection-connattr
    ((connection connection) (k (eql :remote-user)))
  (parse-username-from-id (connection-connattr connection 'id)))

(defmethod connection-connattr
    ((connection connection) (k (eql :remote-uid)))
  (multiple-value-bind (match groups)
      (re:scan-to-strings "^uid=([0-9]+)"
                          (connection-connattr connection 'id))
    (and match (parse-integer (elt groups 0)))))

(defmethod connection-connattr
    ((connection connection) (k (eql :remote-gid)))
  (multiple-value-bind (match groups)
      (re:scan-to-strings "\\) gid=([0-9]+)"
                          (connection-connattr connection 'id))
    (and match (parse-integer (elt groups 0)))))

(defmethod connection-connattr
    ((connection connection) (k (eql :remote-home)))
  "Fetch home directory using tilde expansion, which is POSIX.
Note that looking at $HOME can give the wrong answer when chrooting, as
/etc/passwd inside the chroot is not consulted even for login shells, e.g.
\"chroot /chroot sh -lc 'echo $HOME'\" (we would need something which emulates
login(1)).  Tilde expansion works correctly."
  (multiple-value-bind (home exit)
      (connection-run connection
                      (strcat "echo ~"
                              (connection-connattr connection :remote-user))
                      nil)
    (if (or (string= "" home) (plusp exit))
        (failed-change "Failed to determine remote home directory.")
        (ensure-directory-pathname (stripln home)))))


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
              "~&'~A' failed, exit code ~A~%~%stderr was:~%~A~&~%stdout:~%~A"
              (failed-cmd condition)
              (failed-exit-code condition)
              (failed-stderr condition)
              (failed-stdout condition)))))

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
       (unwind-protect-in-parent
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
#?"umask 077; echo 'mkstemp(${template})' 2>/dev/null | m4 2>/dev/null || mktemp '${template}'"
         nil)
      (let ((lines (lines out)))
        (if (and (zerop exit) lines (plusp (length (car lines))))
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
    `(let ((,previous (get-connattr 'current-directory))
           (,new (ensure-pathname ,dir
                                  :defaults (pwd)
                                  :ensure-absolute t :ensure-directory t)))
       (setf (get-connattr 'current-directory) ,new)
       (unwind-protect
            (if (lisp-connection-p)
                (with-current-directory (,new) ,@forms)
                (progn ,@forms))
         (setf (get-connattr 'current-directory) ,previous)))))

(defun pwd ()
  (or (get-connattr 'current-directory) (get-connattr :remote-home)))

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
     ;; Set HOME (in a way which ENV can override) because with certain
     ;; connection types the value sh(1) sets or inherits is wrong.  E.g. with
     ;; :CHROOT.SHELL we get the value from /etc/passwd outside the chroot.
     ;; Do this unconditionally up here rather than down in the
     ;; implementations of connection types which actually require it for
     ;; simplicity, particularly to avoid having to check whether the connattr
     ;; is set yet, because setting it requires working CONNECTION-RUN.
     (setq cmd (format nil "export HOME=~A; cd ~A; ~A"
                       (escape-sh-token (drop-trailing-slash
                                         (unix-namestring
                                          (get-connattr :remote-home))))
                       (escape-sh-token (unix-namestring (pwd)))
                       cmd))
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
      (informat 4 "~&RUN ~A" cmd)
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

Code in property definitions which will not examine command output should
usually use this in preference to RUN for a performance boost; an exception is
when the command sends a lot of text to stdout which might make it harder for
the user to pick out error messages.  Code which examines command output
should use RUN and only examine the stream from which the output to be read is
expected."
  (%process-run-args
    (informat 4 "~&MRUN ~A" cmd)
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

(defun mountpointp (path)
  "Is PATH a mount point?

Uses mountpoint(1) from util-linux, so add a property requiring OS:LINUX or a
subclass to the :HOSTATTRS subroutine of properties calling this."
  (zerop (mrun :for-exit "mountpoint" "-q" path)))

(defun delete-remote-trees (&rest paths)
  "Recursively delete each of PATHS."
  (mrun "rm" "-rf" paths))

(defun remote-exists-p (&rest paths)
  "Does each of PATHS exists?
PATH may be any kind of file, including directories."
  (apply #'test (loop for path on paths
                      nconc (list "-e" (car path))
                      when (cdr path) collect "-a")))

(defun remote-file-stats (path)
  "Get the numeric mode, size in bytes and mtime of PATH, or NIL if it does not
exist.

The mtime is only accurate to the nearest UTC day, rounding down, if the file
was modified in the past six months or its mtime is in the future, and only
accurate to the nearest minute, rounding down, otherwise (see the
specification of POSIX ls(1))."
  (flet ((sum (chars order)
           (+ (if (char= (elt chars 0) #\r) (* order 4) 0)
              (if (char= (elt chars 1) #\w) (* order 2) 0)
              (eswitch ((elt chars 2) :test #'char=)
                (#\S (if (= order #o100) #o4000 #o2000))
                (#\s (if (= order #o100) #o4100 #o2010))
                (#\T #o1000)
                (#\t (+ order #o1000))
                (#\x order)
                (#\- 0)))))
    (and (remote-exists-p path)
         ;; This is a safe parse of ls(1) given its POSIX specification.
         (let* ((ls (words
                     (run :env '(:LC_ALL "C" :TZ "UTC") "ls" "-ld" path)))
                (lscar (car ls)))
           (values (+ (sum (subseq lscar 1 4) #o100)
                      (sum (subseq lscar 4 7) #o10)
                      (sum (subseq lscar 7 10) 1))
                   (parse-integer (nth 4 ls))
                   (let ((date (parse-integer (nth 6 ls)))
                         (month (cdr
                                 (assoc
                                  (nth 5 ls)
                                  '(("Jan" . 1) ("Feb" . 2) ("Mar" . 3)
                                    ("Apr" . 4) ("May" . 5) ("Jun" . 6)
                                    ("Jul" . 7) ("Aug" . 8) ("Sep" . 9)
                                    ("Oct" . 10) ("Nov" . 11) ("Dec" . 12))
                                  :test #'string=))))
                     (if (find #\: (nth 7 ls))
                         (destructuring-bind (hour minute)
                             (split-string (nth 7 ls) :separator ":")
                           (encode-universal-time
                            0 (parse-integer minute) (parse-integer hour)
                            date month (nth-value 5 (get-decoded-time))
                            0))
                         (encode-universal-time
                          0 0 0 date month (parse-integer (nth 7 ls)) 0))))))))

(defun remote-last-reboot ()
  "Get the time of the last reboot, rounded down to the nearest minute."
  ;; The '-b' option to who(1) is specified in POSIX, though not the output
  ;; format; this parse is based on GNU coreutils who(1).
  (multiple-value-bind (match groups)
      (re:scan-to-strings
       "([0-9]{4})-([0-9]{2})-([0-9]{2}) ([0-9]{2}):([0-9]{2})"
       (car (runlines :env '(:TZ "UTC") "who" "-b")))
    (if match
        (let ((groups (map 'vector #'parse-integer groups)))
          (encode-universal-time 0 (elt groups 4) (elt groups 3)
                                 (elt groups 2) (elt groups 1) (elt groups 0)
                                 0))
        (failed-change "Could not determine time of remote's last reboot."))))

(defun remote-consfigurator-cache-pathname (path)
  (merge-pathnames
   path (car (runlines "echo ${XDG_CACHE_HOME:-$HOME/.cache}/consfigurator/"))))

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
                                (run :env '(:LOCALE "C") "ls" "-nd" pathname))
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

(defun get-connattr (k)
  "Get the connattr identified by K for the current connection."
  (connection-connattr *connection* k))

(defun (setf get-connattr) (v k)
  (setf (connection-connattr *connection* k) v))

(defmacro with-connattrs ((&rest connattrs) &body forms)
  "Execute FORMS with connattrs replaced as specified by CONNATTRS, a plist."
  (with-gensyms (old)
    `(with-slots (connattrs) *connection*
       (let ((,old connattrs))
         (setf connattrs (copy-list connattrs))
         (doplist (k v (list ,@connattrs)) (setf (getf connattrs k) v))
         (unwind-protect (progn ,@forms)
           (setf connattrs ,old))))))
