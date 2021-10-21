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

(in-package :consfigurator)
(named-readtables:in-readtable :consfigurator)

;;;; Remote Lisp images

;;; Remote Lisp images fork right after loading all required ASDF systems.
;;; The parent then enters %CONSFIGURE.  Further connection hops of type
;;; FORK-CONNECTION are established in grandchildren (actually
;;; great-grandchildren) such that (i) if establishing those hops requires
;;; calling things like chroot(2), setuid(2) and setns(2), then the parent
;;; doesn't get stuck in those contexts; and (ii) subdeployments executed in
;;; those contexts will not have access to any secrets the parent might have
;;; read into its memory.
;;;
;;; Similar considerations apply to dumping executables.
;;;
;;; Previously we forked the original process right before chrooting,
;;; setuiding, etc., but this failed to ensure (ii), meant that the parent
;;; could not be multithreaded as it might later need to fork, and required us
;;; to take extra steps when using UNWIND-PROTECT to ensure cleanup forms
;;; weren't executed on both sides of any forks, using a specialised macro.
;;;
;;; Right before establishing the FORK-CONNECTION, the grandchild also
;;; recursively sets up infrastructure to request grandchildren for
;;; establishing further connection hops or dumping executables, such that it
;;; too can be multithreaded even if there are sub-subdeployments, etc..
;;;
;;; We use named pipes for the IPC to minimise implementation-specific code.

(defvar *fork-control* nil)

(defmacro with-fork-request (prerequest request (out err exit) &body forms)
  (with-gensyms (input output)
    `(progn
       (unless (lisp-connection-p)
         (failed-change "Forking requires a Lisp-type connection."))
       (unless *fork-control*
         (failed-change
          "Fork requested but no fork control child; is this the root Lisp?"))
       (informat 3 "~&Making grandchild request ~S" ,request)
       (with-mkfifos (,input ,output)
         ;; We send the path to a named pipe, INPUT, rather than our actual
         ;; request.  That way we can be confident that what we send into the
         ;; (shared) requests pipe will be less than PIPE_BUF (see pipe(7)).
         (write-to-mkfifo (cons ,input ,output) *fork-control*)
         (with-open-file (,input ,input :direction :output :if-exists :append
                                        :element-type 'character)
           (write-to-mkfifo ,prerequest ,input)
           (write-to-mkfifo ,request    ,input))
         (destructuring-bind (,out ,err ,exit)
             (safe-read-file-form ,output :element-type 'character)
           ,@forms)))))

;;; These are the two requests we expect to make of grandchildren: complete
;;; the work of an enclosing call to DEPLOY* or DEPLOY-THESE*, or dump an
;;; image which will evaluate a form.  In the former case we always want to
;;; carry over *HOST*, *CONNECTION* and *CONSFIGURATOR-DEBUG-LEVEL*, and in
;;; the latter case we do not carry over any of these by default.

(defmacro eval-in-grandchild (prerequest request (out err exit) &body forms)
  "Evaluate PREREQUEST and REQUEST, both readably printable Lisp forms, in a
grandchild process.  PREREQUEST and REQUEST must be evaluable using only
definitions established statically by your consfig, or in one of the ASDF
systems upon which your consfig depends.  Then bind OUT, ERR and EXIT to the
stdout, stderr and exit code of that process, respectively, and evaluate
FORMS.

PREREQUEST will be evaluated before the grandchild calls fork(2) to establish
its own infrastructure for subsequent uses of this macro, and REQUEST after.
Thus, PREREQUEST must not start up any threads."
  (flet ((wrap (&rest forms)
           ``(let ((*host* ,*host*)
                   (*connection* ,*connection*)
                   (*no-data-sources* t)
                   (*consfigurator-debug-level* ,*consfigurator-debug-level*))
               ,,@forms)))
    `(with-fork-request
         ,(wrap '`(posix-login-environment
                   ,(get-connattr :remote-user) ,(get-connattr :remote-home))
                prerequest)
         ,(wrap request) (,out ,err ,exit)
       ,@forms)))

(defun dump-consfigurator (filename form)
  (nix:umask #o077)
  (uiop:register-image-restore-hook (lambda () (eval form)) nil)
  (uiop:dump-image filename :executable t))

(defun dump-consfigurator-in-grandchild
    (filename &optional (form `(let ((*no-data-sources* t)
                                     (*connection* ,*connection*)
                                     (*consfigurator-debug-level*
                                       ,*consfigurator-debug-level*))
                                 (with-deployment-report
                                   (with-fork-control
                                     (%consfigure nil ,*host*)))
                                 (fresh-line))))
  "Dump an executable image to FILENAME which will evaluate the readably
printable Lisp form FORM, which defaults to one which will execute the current
deployment.  FORM must be evaluable using only definitions established
statically by your consfig, or in one of the ASDF systems upon which your
consfig depends.

Only :LISP property :APPLY subroutines should call this.

The process which performs the dump will have its umask set to #o077, but
implementation-specific image dumping code might undo this (SBCL, for example,
changes the mode of the file to #o755).  You might want to ensure that the
directory containing FILENAME is locked down."
  (with-fork-request nil `(dump-consfigurator ,filename ',form) (out err exit)
    (declare (ignore out))
    (unless (zerop exit)
      (failed-change "~&Failed to dump image; stderr was ~%~%~A" err))))

(defprop image-dumped :lisp (&optional filename form (always form))
  "Dump an executable image to FILENAME which will evaluate FORM, which must be
evaluable using only definitions established statically by your consfig, or in
one of the ASDF systems upon which your consfig depends.

If FILENAME is nil then use ~/.cache/consfigurator/images/latest, and if FORM
is nil then use one which will execute the current deployment.  Unless ALWAYS,
skip dumping an executable image when we can detect that the deployment is
already running from FILENAME."
  (:desc (if form
             (format nil "Dumped image to evaluate ~S" form)
             "Dumped image to execute current deployment"))
  (:apply
   (let ((file (or filename
                   (ensure-directories-exist
                    (merge-pathnames "consfigurator/images/latest"
                                     (get-connattr :XDG_CACHE_HOME))))))
     (unless (and (not always)
                  (eql :linux (uiop:operating-system))
                  (pathname-equal file (resolve-symlinks "/proc/self/exe")))
       (unless filename
         (nix:chmod (unix-namestring (pathname-directory-pathname file)) #o700))
       (if form
           (dump-consfigurator-in-grandchild file form)
           (dump-consfigurator-in-grandchild file))))
   ;; Return :NO-CHANGE, though we can't detect whether a change was actually
   ;; made: it depends on whether the definitions determining the evaluation
   ;; of FORM, or the definition of this host established by the consfig, was
   ;; or were meaningfully altered since the last deployment which applied
   ;; this property with the same arguments.
   :no-change))

(defmacro with-fork-control (&body forms &aux (fork-control (gensym)))
  `(let ((,fork-control (mkfifo)))
     (forked-progn child
         ;; We use MAPC-OPEN-INPUT-STREAMS because (i) the input streams may
         ;; already have been closed if this is a recursive call; (ii) we
         ;; don't want to close the output streams in the case of *DEBUG-IO*
         ;; and *TERMINAL-IO*; and (iii) there is some ambiguity in the
         ;; standard about closing synonym streams; see
         ;; <https://bugs.launchpad.net/sbcl/+bug/1904257>.
         (loop initially (mapc-open-input-streams
                          #'close *standard-input* *debug-io* *terminal-io*)
               with ,fork-control = (open ,fork-control
                                          :element-type 'character)
               for (input . output) = (handler-case (with-safe-io-syntax ()
                                                      (read ,fork-control))
                                        (end-of-file ()
                                          (close ,fork-control)
                                          (uiop:quit)))
               do (mapc-open-output-streams
                   #'force-output
                   *standard-output* *error-output* *debug-io* *terminal-io*)
               when (zerop (fork))
                 do (nix:setsid)
                    (close ,fork-control)
                    (handle-fork-request input output)
                    (uiop:quit))
       (let ((*fork-control* (open ,fork-control
                                   :direction :output :if-exists :append
                                   :element-type 'character)))
         ;; Opening named pipes for writing blocks on the other end being
         ;; opened for reading, so at this point we know the child has it
         ;; open.  Then delete the filesystem reference right away in case we
         ;; are about to chroot or similar, such that we couldn't do it later.
         (delete-file ,fork-control)
         (unwind-protect (progn ,@forms)
           (close *fork-control*)
           (let ((status (nth-value 1 (nix:waitpid child))))
             (unless
                 (and (nix:WIFEXITED status) (zerop (nix:WEXITSTATUS status)))
               (error "Fork control child did not exit zero."))))))))

;; IPC security considerations
;;
;; The grandchild initially shouldn't have anything in memory other than the
;; ASDF systems we've loaded, and a few bits of IPC information like OUT and
;; ERR.  The INPUT pipe has mode 0600.  So by directly evaluating the first
;; thing we receive all that we're permitting is for a process with the same
;; UID and a sufficiently similar view of the filesystem as us to execute and
;; potentially introspect the consfig.  That should not in itself be a
;; security concern, because the consfig should not contain any secrets.
;;
;; The data we get from INPUT is potentially security-sensitive; for example,
;; specifications of onward connection chains might contain sudo passwords
;; (though this would be an unusual way to use Consfigurator).  Another writer
;; to the pipe might insert a reference to the #. reader macro which causes us
;; to reveal what we get from INPUT, or another reader from the pipe might be
;; able to get some of INPUT.  Again, however, only an attacker who has
;; already managed to change to our UID or otherwise circumvent normal POSIX
;; permissions could do any of this.  We might consider encrypting the data we
;; send down the named pipes using a pre-shared key.
;;
;; An alternative to forking would be to dump an image which we reexecute each
;; time we would have created another grandchild; then we can send the request
;; on stdin.  That would mean writing ~75MB out to disk every time we start up
;; a remote Lisp image and every time we establish a further FORK-CONNECTION,
;; however, though perhaps we could arrange to avoid dumping an executable if
;; we know that we aren't going to need to reinvoke it, by having connection
;; types push hostattrs.
;;
;; If we took this approach, then we'd have implementation-specific dumping
;; code, but the code to reinvoke the dumped images would be fully portable --
;; so perhaps we would want to always begin by dumping an image to a temporary
;; file (which wouldn't require forking), and then we immediately reinvoke it
;; to perform the deployment using implementation-agnostic code.  In place of
;; :SETUID connections we might runuser(1) the image, which would have the
;; advantage of getting us a fresh PAM session, although it would mean making
;; the executable readable by the target user.
(defun handle-fork-request (input output &aux (out (mkfifo)) (err (mkfifo)))
  (forked-progn child
      (with-backtrace-and-exit-code
        ;; Capture stdout and leave it to the request submitter to decide what
        ;; to do with it, because perhaps the requester has rebound
        ;; *STANDARD-OUTPUT*, e.g. in an enclosing call to APPLY-AND-PRINT.
        ;;
        ;; Similarly for stderr.  In particular, we discard the stderr from
        ;; remote Lisp images unless they fail due to an unhandled error, so
        ;; if we just leave stderr uncaptured then it might be the case that
        ;; the user will never see it.  Also see commit 9e7ae48590.
        (with-open-file (*standard-output* out :direction :output
                                               :if-exists :append
                                               :element-type 'character)
          (with-open-file (*error-output* err :direction :output
                                              :if-exists :append
                                              :element-type 'character)
            ;; Try to ensure that the new fork control child does not end up
            ;; with the actual request in its memory.
            (with-open-file (input input :element-type 'character)
              (flet ((eval-input ()
                       (eval
                        (with-standard-io-syntax (slurp-stream-form input)))))
                (eval-input)
                (with-fork-control (eval-input)))))))
    (unwind-protect
         (let* (outbuf
                (out-reader
                  (bt:make-thread
                   (lambda ()
                     (setq outbuf
                           (read-file-string out :element-type 'character)))))
                errbuf
                (err-reader
                  (bt:make-thread
                   (lambda ()
                     (setq errbuf
                           (read-file-string err :element-type 'character)))))
                (status (nth-value 1 (nix:waitpid child))))
           (unless (nix:WIFEXITED status)
             (failed-change
              "~&Grandchild process did not exit normally, status #x~(~4,'0X~)."
              status))
           (bt:join-thread out-reader)
           (bt:join-thread err-reader)
           (with-open-file (output output :direction :output
                                          :if-exists :append
                                          :element-type 'character)
             (write-to-mkfifo
              (list outbuf errbuf (nix:WEXITSTATUS status)) output)))
      (delete-file out) (delete-file err))))

(defclass asdf-requirements ()
  ((asdf-requirements :type list :initform nil))
  (:documentation
   "A list of requirements as returned by certain calls to
ASDF:REQUIRED-COMPONENTS.
Elements are instances of ASDF:SYSTEM and/or ASDF:REQUIRE-SYSTEM."))

(defun asdf-requirements-for-host-and-features (remote-lisp-features)
  "Make an instance of ASDF-REQUIREMENTS for starting up a remote Lisp image in
which *FEATURES* has the value of REMOTE-LISP-FEATURES, based on the Lisp
systems required by the host currently being deployed.

Called by connection types which start up remote Lisp images."
  (let ((*features* remote-lisp-features)
        (requirements (make-instance 'asdf-requirements)))
    (with-slots (asdf-requirements) requirements
      (dolist (system (propspec-systems (host-propspec *host*)))
        ;; This call to ASDF:REQUIRED-COMPONENTS seems to get us everything,
        ;; but see the warning in the comment attached to the call to
        ;; ASDF:REQUIRED-COMPONENTS in the (defmethod component-depends-on ((o
        ;; gather-operation) (s system))) implementation in ASDF's source.
        (dolist (requirement
                 (asdf:required-components
                  (asdf:find-system system)
                  :other-systems t :keep-component 'asdf:system
                  :goal-operation 'asdf:monolithic-compile-bundle-op))
          (let ((name (asdf:component-name requirement)))
            ;; Handle UIOP specially because it comes with ASDF.
            (unless
                (memstring= (asdf:primary-system-name name) '("asdf" "uiop"))
              (pushnew requirement asdf-requirements)))))
      (nreversef asdf-requirements))
    requirements))

(defgeneric request-asdf-requirements (asdf-requirements)
  (:documentation
   "Request that all Lisp systems required to fulfill ASDF-REQUIREMENTS be
uploaded to the remote cache of the currently established connection.

Called by connection types which start up remote Lisp images.")
  (:method ((asdf-requirements asdf-requirements))
    (loop for requirement in (slot-value asdf-requirements 'asdf-requirements)
          for type = (type-of requirement)
          when (and (subtypep type 'asdf:system)
                    (not (subtypep type 'asdf:require-system)))
            do (require-data "--lisp-system"
                             (asdf:primary-system-name requirement)))))

(defgeneric asdf-requirements-load-form (asdf-requirements)
  (:documentation
   "Return form to unpack and load each of the Lisp systems specified in
ASDF-REQUIREMENTS, after having uploaded those Lisp systems using
UPLOAD-ALL-PREREQUISITE-DATA.")
  (:method ((asdf-requirements asdf-requirements))
    `(progn
       (let* ((cache (uiop:xdg-cache-home))
              (dest (ensure-directories-exist
                     (merge-pathnames "consfigurator/systems/" cache)))
              (file (merge-pathnames
                     "consfigurator/extracted-systems" cache))
              (record (and (file-exists-p file)
                           (safe-read-file-form file))))
         (unwind-protect
              (with-current-directory (dest)
                ,@(loop
                    for (iden1 . system) being the hash-keys
                      in (get-connattr 'cached-data) using (hash-value tarball)
                    for version = (parse-integer (pathname-name tarball))
                    and system* = (ensure-directory-pathname system)
                    when (string= "--lisp-system" iden1)
                      collect
                    `(let ((pair (assoc ,system record :test #'string=)))
                       (unless (and pair (>= (cdr pair) ,version))
                         (when (directory-exists-p ,system*)
                           (delete-directory-tree
                            ,system*
                            :validate
                            (lambda (dir)
                              (and (relative-pathname-p dir)
                                   (not (search ".." (unix-namestring dir)))))))
                         (run-program
                          (list "tar" "-C" (ensure-directories-exist
                                            ,(unix-namestring system*))
                                "-xzf" ,(unix-namestring tarball)))
                         (if pair
                             (rplacd pair ,version)
                             (setq record (acons ,system ,version record)))))))
           (with-open-file (stream file :direction :output
                                        :if-exists :supersede)
             (with-standard-io-syntax (prin1 record stream))))
         (asdf:clear-source-registry)
         (asdf:initialize-source-registry
          `(:source-registry (:tree ,dest) :ignore-inherited-configuration)))
       ,@(loop for system in (propspec-systems (host-propspec *host*))
               collect `(asdf:load-system ,system)))))

(defgeneric continue-deploy*-program (remaining-connections asdf-requirements)
  (:documentation
   "Return a program to complete the work of an enclosing call to DEPLOY*.

Implementations of ESTABLISH-CONNECTION which start up remote Lisp images call
this function, instead of CONTINUE-DEPLOY*, and use the result to instruct the
newly started image.

Will query the remote cache for paths to Lisp systems, so a connection to the
host which will run the Lisp image must already be established.

The program returned is a single string consisting of a number of sexps
separated by newlines.  Each sexp must be evaluated by the remote Lisp image
before the following sexp is offered to its reader, on standard input.")
  (:method (remaining-connections (asdf-requirements asdf-requirements))
    (unless (eq (type-of *host*) 'preprocessed-host)
      (error "Attempt to send unpreprocessed host to remote Lisp.

Preprocessing must occur in the root Lisp."))
    (flet ((wrap (form)
             ;; We used to bind a handler here to invoke SKIP-DATA-SOURCES
             ;; upon MISSING-DATA-SOURCE, which means that remote Lisp images
             ;; were allowed to try querying data sources.  Now we just bind
             ;; *NO-DATA-SOURCES* to t here.  While some data sources make
             ;; sense in remote Lisp images, others might make arbitrary
             ;; network connections or read out of other users' homedirs
             ;; (e.g. if you are using (:SUDO :SBCL), the remote Lisp might
             ;; try to read your ~/.gnupg, or on another host, someone else's
             ;; ~/.gnupg who has the same username as you), which are usually
             ;; undesirable.  So at least until some cool use case comes
             ;; along, just require all data source queries to occur in the
             ;; root Lisp.
             `(let ((*no-data-sources* t)
                    (*consfigurator-debug-level* ,*consfigurator-debug-level*))
                ,form)))
      (let* ((intern-forms
               (loop for (export . name)
                       in '((nil . "*NO-DATA-SOURCES*")
                            (t . "*CONSFIGURATOR-DEBUG-LEVEL*"))
                     for intern-form
                       = `(intern ,name (find-package "CONSFIGURATOR"))
                     if export collect
                       `(export ,intern-form (find-package "CONSFIGURATOR"))
                     else collect intern-form))
             (proclamations `((proclaim '(special *no-data-sources*))
                              (proclaim '(special *consfigurator-debug-level*))))
             (forms
               `((make-package "CONSFIGURATOR")
                 ,@intern-forms
                 ,@proclamations
                 ;; (define-condition missing-data-source (error) ())
                 (require "asdf")
                 ;; Hide the compile and/or load output unless there are
                 ;; failures or the debug level is at least 3, as it's verbose
                 ;; and not usually of interest.
                 ,(wrap
                   `(let ((string
                            (make-array '(0) :element-type 'character
                                             :fill-pointer 0 :adjustable t)))
                      (handler-case
                          (with-output-to-string (stream string)
                            (let ((*error-output* stream)
                                  (*standard-output* stream)
                                  (*debug-io*
                                    (make-two-way-stream *debug-io* stream)))
                              ,(asdf-requirements-load-form
                                asdf-requirements)))
                        (serious-condition (c)
                          (format
                           *error-output*
                           "~&Failed to compile and/or load:~%~A~&~%Compile and/or load output:~%~%~A"
                           c string)
                          (uiop:quit 3)))
                      (when (>= *consfigurator-debug-level* 3)
                        (format t "~&~A" string))))
                 ;; Continue the deployment.  The READ indirection is to try
                 ;; to ensure that the fork control child does not end up with
                 ;; information about the deployment in its memory.
                 ,(wrap `(with-backtrace-and-exit-code
                           (with-fork-control (eval (read)))))
                 (%consfigure ',remaining-connections ,*host*))))
        (handler-case
            (with-standard-io-syntax
              (let ((*allow-printing-passphrases* t))
                ;; need line breaks in between so that packages exist before we
                ;; try to have remote Lisp read sexps containing symbols from
                ;; those packages
                (values
                 (format nil "~{~A~^~%~}" (mapcar #'prin1-to-string forms))
                 forms)))
          (print-not-readable (c)
            (error "The Lisp printer could not serialise ~A for
transmission to the remote Lisp.

This is probably because your property application specification and/or static
informational attributes contain values which the Lisp printer does not know
how to print.  If ~:*~A is something like a function object then you need to
rework your deployment so that it does not end up in the propspec or
hostattrs; see \"Pitfalls\" in the Consfigurator user manual.

If ~:*~A is a simple object then you may be able to resolve this by defining
a PRINT-OBJECT method for your class, possibly using
CONSFIGURATOR:DEFINE-PRINT-OBJECT-FOR-STRUCTLIKE."
                   (print-not-readable-object c))))))))
