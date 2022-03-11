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

(in-package :consfigurator.connection.linux-namespace)
(named-readtables:in-readtable :consfigurator)

;;; 'SETNS is SBCL-specific due to handling of non-user threads like the
;;; finaliser thread: we must be truly single-threaded in order to enter a
;;; different user namespace.  If we can't use it then we fall back to a
;;; POSIX-type connection into the container.  In the latter situation the
;;; user could follow the :SYSTEMD-MACHINED/:LXC/etc. connection with a
;;; connection type which starts up a remote Lisp image within the container.
;;; This will be significantly slower, so if there is more than one container
;;; it will probably be worth arranging for the use of 'SETNS.
;;;
;;; For containers which can be launched only by root, like systemd-nspawn, we
;;; use nsenter(1) for the POSIX-type connection into them.  For containers
;;; owned by non-root, however, we use container-specific commands like
;;; lxc-unpriv-attach(1).  This is because nsenter(1) with --all can fail with
;;; permission errors depending on which namespaces are had in common inside
;;; and outside of the container.  For example, if the container has the same
;;; time namespace, nsenter(1) will fail to re-enter it because the non-root
;;; user lacks CAP_SYS_ADMIN for the user namespace owning that time
;;; namespace.  'SETNS handles this case by ignoring EPERM in the second pass.
;;;
;;; An alternative to calling setns(2) ourselves, in the Lisp-type connection
;;; case, might be to dump an image, write it to a temporary file within the
;;; container using WITH-REMOTE-TEMPORARY-FILE & nsenter(1), and then reinvoke
;;; the image, again using nsenter(1), or using lxc-unpriv-attach(1), etc.
;;; This would be more portable with respect to Lisp implementations.
;;; However, it would run into the problems described in "Dumping and
;;; reinvoking Lisp" in pitfalls.rst.  With the current approach, shared
;;; library dependencies need be available only outside the container, and
;;; then after entering the container they will still be usable.

(defclass linux-namespace-connection ()
  ((name :type string :initarg :name
         :documentation
         "The name of the container as output by commands like lxc-ls(1).")
   (pid :type integer :initarg :pid
        :initform (simple-program-error "Must supply namespace leader PID.")
        :documentation
        "A PID of a process which is already within all of the namespaces.")
   (env :type list :initform nil)
   (uid :initarg :uid :initform nil)
   (gid :initarg :gid :initform nil))
  (:documentation
   "A connection which works by reassociating to a set of Linux namespaces."))

(defmethod initialize-instance :after
    ((connection linux-namespace-connection) &key)
  (with-slots (pid env) connection
    ;; In the case that PID is in another user namespace, we won't be able to
    ;; read the environment unless we're either root or we first enter the
    ;; other user namespace.
    (let ((cmd
#?"cat /proc/${pid}/environ || nsenter -U -t ${pid} cat /proc/${pid}/environ"))
      (dolist (entry (split-string (run cmd) :separator '(#.(code-char 0))))
        (when (plusp (length entry))
          (push entry env))))))


;;;; :NSENTER

(defclass nsenter-connection
    (linux-namespace-connection shell-wrap-connection) ())

(defmethod connection-shell-wrap ((connection nsenter-connection) cmd)
  (with-slots (pid uid gid env) connection
    (format
     nil
     "nsenter ~@[-S ~D ~]~@[-G ~D ~]-at ~D env -i ~{~A~^ ~} sh -c ~A"
     uid gid pid (mapcar #'sh-escape env) (sh-escape cmd))))

(defmethod establish-connection
    ((type (eql :nsenter)) remaining &key name pid uid gid)
  (declare (ignore remaining))
  (informat 1 "~&Entering namespaces of PID ~D with nsenter(1)" pid)
  (make-instance 'nsenter-connection :name name :pid pid :uid uid :gid gid))

(defmethod establish-connection
    ((type (eql :systemd-machined)) remaining &key name uid gid)
  (let ((type #+sbcl (if (lisp-connection-p) 'setns :nsenter) #-sbcl :nsenter)
        (pid (or (loop for line in (runlines "machinectl" "show" name)
                       when (string-prefix-p "Leader=" line)
                         return (subseq line 7))
                 (error "Could not determine PID for container ~A." name))))
    (apply #'establish-connection type remaining
           :name name :pid pid :uid uid :gid gid
           (and (eql 'setns type) '(:posix-type nsenter-connection)))))


;;;; :LXC-UNPRIV-ATTACH

(defclass lxc-unpriv-attach-connection
    (linux-namespace-connection shell-wrap-connection)
  ((owner :initarg :owner :initform nil)
   (owner-uid :initarg :owner-uid :initform nil)))

(defmethod initialize-instance :after
    ((connection lxc-unpriv-attach-connection) &key)
  (with-slots (owner owner-uid) connection
    (when owner (setf owner-uid (user:passwd-entry 2 owner)))))

(defmethod connection-shell-wrap
    ((connection lxc-unpriv-attach-connection) cmd)
  (with-slots (owner owner-uid name uid gid env) connection
    ;; Here we reimplement lxc-unpriv-attach(1) in order to pass --quiet to
    ;; systemd-run(1), else we get extra output on stderr.
    (let ((args `("systemd-run" "--scope" "--quiet" "-p" "Delegate=yes"
                  "/usr/bin/lxc-attach" "-n" ,name "--clear-env"
                  ,@(loop for env in env collect "--set-var" collect env)
                  ,@(and uid `("-u" ,(write-to-string uid)))
                  ,@(and gid `("-g" ,(write-to-string gid)))
                  "--" "sh" "-c" ,cmd)))
      (if (and owner (not (string= owner (get-connattr :remote-user))))
          (with-connattrs (:remote-uid owner-uid)
            (list* "runuser" "-u" owner "--" (apply #'systemd--user args)))
          (apply #'systemd--user args)))))

(defmethod establish-connection
    ((type (eql :lxc-unpriv-attach)) remaining &key owner name pid uid gid)
  (declare (ignore remaining))
  (informat 1 "~&Entering namespaces of PID ~D with lxc-unpriv-attach(1)" pid)
  (make-instance 'lxc-unpriv-attach-connection
                 :owner owner :name name :pid pid :uid uid :gid gid))

(defmethod establish-connection
    ((type (eql :lxc)) remaining
     &key owner (name (and (not remaining) (get-hostname))) uid gid)
  "Attach to the LXC named NAME and owned by OWNER, defaulting to the current
user.  Switch to UID and GID inside the LXC.

When the previously established connection hop is a Lisp-type connection, this
connection type will dump and reinvoke Lisp.  Thus, connections established
since the Lisp image was started up but before this one must not have rendered
the original ~/.cache/common-lisp/ unreadable, or the reinvoked image will
fail to start.  For example,

    (:ssh :sbcl (:lxc :name \"foo\"))

and

    ((:ssh :user \"root\") :sbcl (:lxc :owner \"user\" :name \"foo\"))

will work but

    ((:ssh :user \"root\") :sbcl (:setuid :to \"user\") (:lxc :name \"foo\"))

will not.  See \"Dumping and reinvoking Lisp\" in the \"Pitfalls and
limitations\" section of the Consfigurator manual.

When the current connection is a Lisp-type connection, this internally uses
setns(2) to enter the container.  See \"Connections which use setns(2) to
enter containers\" in the Consfigurator manual for security implications."
  (let ((type #+sbcl (if (lisp-connection-p) 'setns :lxc-unpriv-attach)
              #-sbcl :lxc-unpriv-attach)
        (pid (loop
               for (lxc pid)
                 in (mapcar #'words (cdr (lxc:lxc-ls owner "-1fFNAME,PID")))
               when (string= lxc name)
                 return pid
               finally
                  (error "Could not determine PID for container ~A." name))))
    (apply #'establish-connection type remaining
           :owner owner :name name :pid pid :uid uid :gid gid
           (and (eql 'setns type)
                '(:posix-type lxc-unpriv-attach-connection)))))


;;;; 'SETNS

;;; Whether to setuid/setgid: setting aside --preserve-credentials, which we
;;; don't support, nsenter(1) will setuid (resp. setgid) when a UID
;;; (resp. GID) is supplied on the command line, or when asked to enter a new
;;; user namespace, either with -U or -t.  In the latter case both default to
;;; 0.  So that we can abstract over 'SETNS and :NSENTER using a connection
;;; type like :SYSTEMD-MACHINED, we mirror these semantics in our 'SETNS.
#+sbcl
(defmethod establish-connection
    ((type (eql 'setns)) remaining
     &rest args &key pid posix-type &allow-other-keys)
  "Use setns(2) to enter the Linux namespaces of process PID.  Additionally,

- If PID has a distinct user namespace and we have permission to setgroups(2)
  in the initial user namespace, then before entering the target userns,

  - if the target userns is owned by root, clear supplementary groups

  - if the target userns is owned by nonroot, call initgroups(3) to assume the
    supplementary groups of the owner.

- After entering the target namespaces:

  - If UID, or PID has a distinct userns, attempt to setuid(2) to UID, in the
    latter case defaulting UID to 0.  Also change to UID's home directory, and
    update HOME, PATH, USER and LOGNAME environment variables.

  - If GID, or PID has a distinct userns, attempt to setgid(2) to GID, in the
    latter case defaulting GID to 0.  Also, if setgroups(2) is permitted
    within the target user namespace,

    - if we also called setuid(2) then call initgroups(3) to assume the
      supplementary groups belonging to UID

    - if we called only setgid(2), clear supplementary groups.

Thus, if PID has a distinct userns then the userns's uid_map and gid_map must
already have been written, and must include mappings for UID and GID, which
default to 0 and 0.  It is not an error if we do not have the ability to
setgroups(2) in either the starting user namespace or the target user
namespace, in each case either due to a lack of privilege or because
setgroups(2) is denied in the namespace."
  (informat 1 "~&Reassociating to Linux namespaces of PID ~D" pid)
  (alet (apply #'make-instance posix-type (remove-from-plist args :posix-type))
    (upload-all-prerequisite-data it)
    (change-class it 'setns-connection)
    (continue-connection it remaining)))

(defclass setns-connection
    (linux-namespace-connection init-hooks-connection) ())

#+linux
(define-constant +namespace-types+ `(("user"   . ,+CLONE_NEWUSER+)
                                     ("cgroup" . ,+CLONE_NEWCGROUP+)
                                     ("ipc"    . ,+CLONE_NEWIPC+)
                                     ("uts"    . ,+CLONE_NEWUTS+)
                                     ("net"    . ,+CLONE_NEWNET+)
                                     ("pid"    . ,+CLONE_NEWPID+)
                                     ("mnt"    . ,+CLONE_NEWNS+)
                                     ,@(and (boundp '+CLONE_NEWTIME+)
                                            `(("time" . ,+CLONE_NEWTIME+))))
  :test #'equal)

(define-error-retval-cfun () "setns" :int (fd :int) (type :int))

#+(and linux sbcl)
(defmethod post-fork ((connection setns-connection))
  (with-slots (pid uid gid env) connection
    (let* (user opened-fds
           ;; Check whether the target user namespace is the current user
           ;; namespace because it is never permitted to reenter the current
           ;; user namespace using setns(2).
           (us (nix:stat #?"/proc/${(nix:getpid)}/ns/user"))
           (them (nix:stat #?"/proc/${pid}/ns/user"))
           (setuserns (not (and (= (nix:stat-dev us) (nix:stat-dev them))
                                (= (nix:stat-ino us) (nix:stat-ino them)))))
           (uid (or uid (and setuserns 0)))
           (gid (or gid (and setuserns 0))))
      (unwind-protect
           (flet ((sysopen (path)
                    (aprog1 (nix:open path nix:O-RDONLY)
                      (push it opened-fds))))
             (let ((ns-fds
                     (loop for (name . type) in (if setuserns
                                                    +namespace-types+
                                                    (cdr +namespace-types+))
                           collect
                           (cons (sysopen #?"/proc/${pid}/ns/${name}") type)))
                   (root-fd (sysopen #?"/proc/${pid}/root")))
               (when (and setuserns (setgroups-p))
                 (let ((owner (get-userns-owner (caar ns-fds))))
                   (if (zerop owner)
                       (nix:setgroups nil)
                       (alet (osicat:user-info owner)
                         ;; As a precaution, we could also setuid & setgid to
                         ;; OWNER here.  However, it ought to be meaningless
                         ;; to do so, because a process loses all capabilities
                         ;; in the parent or previous user namespace.
                         (nix:initgroups (cdr (assoc :name it))
                                         (cdr (assoc :group-id it)))))))
               ;; Reset to a standard SHELL and PATH & clear out rest of env.
               (posix-login-environment)
               ;; It might be that we need to enter the userns in order to
               ;; have the capability to enter the other namespaces, or it
               ;; might be that we want to enter the userns in order to reduce
               ;; our privilege.  So that we can handle both of these, try
               ;; entering the other namespaces both before and after entering
               ;; the new user namespace; the first set of attempts will fail
               ;; silently in the former case.
               ;;
               ;; This technique is based on part of nsenter(1) from
               ;; util-linux, but additionally, in the first pass we accept
               ;; errors only of type EPERM, and see below about EPERM in the
               ;; second pass.
               (when setuserns
                 (loop for cell on (cdr ns-fds)
                       do (handler-case (setns (caar cell) (cdar cell))
                            (nix:eperm ())
                            (:no-error (&rest ignore)
                              (declare (ignore ignore))
                              (rplaca (car cell) nil)))))
               (loop for (fd . type) in ns-fds
                     ;; Accept failures due to insufficient capabilities which
                     ;; occur after we've entered the new userns, as that
                     ;; indicates that the namespace we tried to join belongs
                     ;; to a parent userns, in which case if we were ever
                     ;; going to join it would have to have been on 1st pass.
                     if (and fd setuserns (not (eql type +CLONE_NEWUSER+)))
                       do (handler-case (setns fd type) (nix:eperm ()))
                     else if fd do (setns fd type))
               ;; If we entered new PID or time namespaces then need to fork
               ;; so we're actually within them; for simplicity, always fork.
               (mapc-open-output-streams
                #'force-output
                *standard-output* *error-output* *debug-io* *terminal-io*)
               (let ((child (nix:fork)))
                 (when (plusp child)
                   (let ((status (nth-value 1 (nix:waitpid child))))
                     (if (nix:WIFEXITED status)
                         (uiop:quit (nix:WEXITSTATUS status))
                         (error
                          "PID namespace child did not exit normally.")))))
               ;; If the namespace leader is chrooted then we want to be too.
               (nix:fchdir root-fd) (chroot ".")))
        (mapc #'nix:close opened-fds))
      (when uid
        (alet (or (osicat:user-info uid)
                  (error "~&Could not look up user info for UID ~A." uid))
          (setf user (cdr (assoc :name it)))
          (posix-login-environment uid user (cdr (assoc :home it)))))
      (dolist (entry env)
        (let* ((pos (position #\= entry))
               (var (subseq entry 0 pos))
               (val (subseq entry (1+ pos))))
          (unless
              (and uid
                   (memstr= var '("HOME" "SHELL" "USER" "LOGNAME" "PATH")))
            (setf (getenv var) val))))
      (when gid
        (nix:setgid gid)
        (when (setgroups-p)
          (if uid (nix:initgroups user gid) (nix:setgroups nil))))
      (when uid (nix:setuid uid)))))
