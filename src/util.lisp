;;; Consfigurator -- Lisp declarative configuration management system

;;; Copyright (C) 2020-2022  Sean Whitton <spwhitton@spwhitton.name>

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

(defun multiple-value-mapcan (function &rest lists)
  "Variant of MAPCAN which preserves multiple return values."
  (let ((lists (copy-list lists))
        (results (make-array '(2) :initial-element nil :adjustable t)))
    (loop for new-results
            = (multiple-value-list
               (apply function
                      (loop for list on lists
                            if (car list)
                              collect (pop (car list))
                            else do (return-from multiple-value-mapcan
                                      (values-list (coerce results 'list))))))
          do (adjust-array results (max (length results) (length new-results))
                           :initial-element nil)
             (loop for result in new-results and i upfrom 0
                   do (nconcf (aref results i) result)))))

(defun lines (text &optional trimfun (trimchars '(#\Space #\Tab)))
  (with-input-from-string (stream text)
    (let (bolp buffer)
      (flet ((trim (line)
               (if trimfun (funcall trimfun trimchars line) line))
             (reset ()
               (setq bolp t
                     buffer (make-array 0 :fill-pointer 0
                                          :element-type 'character))))
        ;; Split on either <CR>, <LF> or <CR><LF>; <LF><CR> would mean split
        ;; with a blank line in between.  Drop a single trailing blank line.
        (loop initially (reset)
              for char = (read-char stream nil nil)
              if char
                if (member char '(#\Return #\Newline) :test #'char=)
                  collect (trim buffer)
                  and do (reset)
                         (when (char= char #\Return)
                           (when-let ((next (peek-char nil stream nil nil)))
                             (when (char= next #\Newline)
                               (read-char stream))))
                else do (setq bolp nil)
                        (vector-push-extend char buffer)
                end
              else
                unless bolp collect (trim buffer) end
                and do (loop-finish))))))

(defun unlines (lines)
  (format nil "~{~A~%~}" lines))

(defun words (text)
  (delete "" (split-string text) :test #'string=))

(defun unwords (words)
  (format nil "~{~A~^ ~}" words))

(defun memstr= (string list)
  (member string list :test #'string=))

(defun assert-ordinary-ll-member (arg)
  "Assert that ARG is not an implementation-specific lambda list keyword or a
lambda list keyword which is not permitted in ordinary lambda lists.

Consfigurator's property-writing macros do not support lambda list keywords
which fail this assertion."
  (or
   (not
    (member arg
            '#.(set-difference lambda-list-keywords
                               '(&optional &rest &key &allow-other-keys &aux))))
   (simple-program-error
    "Implementation-specific or non-ordinary lambda list keyword ~A not
supported."
    arg)))

(defun ordinary-ll-without-&aux (ll)
  (loop for arg in ll
        do (assert-ordinary-ll-member arg)
        if (eq '&aux arg) return accum
          else collect arg into accum
        finally (return accum)))

(defun ordinary-ll-variable-names (ll &key include-supplied-p)
  (loop for arg in ll
        for arg* = (ensure-car arg)
        do (assert-ordinary-ll-member arg)
        unless (char= #\& (char (symbol-name arg*) 0))
          collect arg*
          and when (and include-supplied-p (listp arg) (caddr arg))
                collect (caddr arg)))

(defmacro defun-with-args (name argsym lambda-list &body forms &aux remaining)
  (multiple-value-bind (required optional rest kwargs aokeys)
      (parse-ordinary-lambda-list lambda-list)
    (when (and aokeys (not rest))
      (simple-program-error
       "&ALLOW-OTHER-KEYS without &REST in property lambda list not supported."))
    (let ((normalisedll (reverse required)))
      (when optional
        (push '&optional normalisedll)
        (loop for (name init suppliedp) in optional
              for suppliedp* = (or suppliedp (gensym))
              do (push `(,name ,init ,suppliedp*) normalisedll)
              do (push `(when ,suppliedp* (push ,name ,argsym)) remaining)))
      (when rest
        (push '&rest normalisedll)
        (push rest normalisedll)
        (push `(dolist (r ,rest) (push r ,argsym)) remaining))
      (when kwargs
        (push '&key normalisedll)
        (loop for ((keyword-name name) init suppliedp) in kwargs
              for suppliedp* = (if (or rest suppliedp) suppliedp (gensym))
              do (push `((,keyword-name ,name) ,init ,suppliedp*)
                       normalisedll)
              unless rest do (push `(when ,suppliedp*
                                      (push ,keyword-name ,argsym)
                                      (push ,name ,argsym))
                                   remaining)))
      (when aokeys
        (push '&allow-other-keys normalisedll))
      `(defun ,name ,(nreverse normalisedll)
         (let ((,argsym (list ,@(reverse required))))
           ,@(nreverse remaining)
           (nreversef ,argsym)
           ,@forms)))))

(defmacro define-simple-error (name &optional parent-types docstring)
  `(progn
     (define-condition ,name (,@parent-types simple-error) ()
       ,@(and docstring `((:documentation ,docstring))))
     (defun ,name (message &rest args)
       ,@(and docstring `(,docstring))
       (error ',name :format-control message :format-arguments args))))

(defmacro form-beginning-with (sym form)
  `(and (listp ,form)
	(symbolp (car ,form))
	(string= (symbol-name (car ,form)) ,(symbol-name sym))))

(defun strip-declarations (forms)
  (loop while (form-beginning-with declare (car forms))
        do (pop forms)
        finally (return forms)))

(defun plist-to-long-options (plist &aux args)
  (doplist (k v plist args)
           (push (strcat "--" (string-downcase (symbol-name k)) "=" v) args)))

(defun systemd-user-instance-args (args)
  "Where ARGS are args to RUN or MRUN for an invocation of a systemd command
which can take \"--user\", insert the \"--user\" parameter, and modify or
insert an :ENV parameter so that the call is more likely to succeed."
  (loop with xrd = (format nil "/run/user/~D" (get-connattr :remote-uid))
        with dsba = (format nil "unix:path=~A/bus" xrd)
        with arg-done and env-done while args
        as next = (pop args) collect next into accum
        if (and (not arg-done) (stringp next))
          collect "--user" into accum and do (setq arg-done t)
        if (eql :env next)
          collect (aprog1 (copy-list (pop args))
                    (setf (getf it :XDG_RUNTIME_DIR) xrd
                          (getf it :DBUS_SESSION_BUS_ADDRESS) dsba
                          env-done t))
            into accum
        if (eql :input next)
          collect (pop args)
        finally (return (if env-done
                            accum
                            (list* :env `(:XDG_RUNTIME_DIR ,xrd
                                          :DBUS_SESSION_BUS_ADDRESS ,dsba)
                                   accum)))))

(defmacro with-local-temporary-directory ((dir) &body forms)
  "Execute FORMS with a local temporary directory's pathname in DIR.
Currently assumes GNU mktemp(1).

There is no WITH-REMOTE-TEMPORARY-DIRECTORY because POSIX doesn't include a
shell utility to create temporary directories.  If you need a remote temporary
directory, one solution is to convert your property to a :LISP property."
  `(let ((,dir (ensure-directory-pathname
		(stripln
		 (run-program "umask 077; mktemp -d" :output :string)))))
     (unwind-protect (progn ,@forms)
       (delete-directory-tree ,dir :validate t))))

(defun pathname-file (pathname)
  "Like PATHNAME-NAME but include any file extension."
  (and (pathname-name pathname)
       (namestring
        (if (pathname-directory pathname)
            (enough-pathname pathname (pathname-directory-pathname pathname))
            pathname))))

(defun local-directory-contents (pathname)
  "Return the immediate contents of PATHNAME, a directory, without resolving
symlinks.  Not suitable for use by :POSIX properties."
  ;; On SBCL on Debian UIOP:*WILD-FILE-FOR-DIRECTORY* is #P"*.*".
  (uiop:directory*
   (merge-pathnames uiop:*wild-file-for-directory*
                    (ensure-directory-pathname pathname))))

(defun ensure-trailing-slash (namestring)
  (if (string-suffix-p namestring "/")
      namestring
      (strcat namestring "/")))

(defun drop-trailing-slash (namestring)
  (if (string-suffix-p namestring "/")
      (subseq namestring 0 (1- (length namestring)))
      namestring))

(defun reinit-from-simple-print (class &rest slots)
  (aprog1 (allocate-instance (find-class class))
    (loop for (slot-name slot-value) on slots by #'cddr
          do (setf (slot-value it slot-name) slot-value))))

(defmacro quote-nonselfeval (x)
  (once-only (x)
    `(if (member (type-of ,x) '(cons symbol))
         `',,x ,x)))

(defmacro define-simple-print-object (class)
  "Define an implementation of PRINT-OBJECT suitable for classes representing
simple collections of readably-printable values."
  `(defmethod print-object ((object ,class) stream)
     (if (and *print-readably* *read-eval*)
         (format
          stream "#.~S"
          `(reinit-from-simple-print
            ',(type-of object)
            ;; Call CLASS-OF so that subclasses of CLASS are handled too.
            ,@(loop for slot in (closer-mop:class-slots (class-of object))
                    for slot-name = (closer-mop:slot-definition-name slot)
                    when (slot-boundp object slot-name)
                      collect `',slot-name
                      and collect (quote-nonselfeval
                                   (slot-value object slot-name)))))
         (call-next-method))
     object))

(defun chroot-pathname (pathname chroot)
  (merge-pathnames (enough-pathname pathname #P"/")
                   (ensure-directory-pathname chroot)))

(defun in-chroot-pathname (pathname chroot)
  (ensure-pathname (enough-pathname pathname chroot)
                   :ensure-absolute t :defaults #P"/"))

(defun sh-escape (token-or-cmd &optional s)
  (cond ((listp token-or-cmd) (uiop:escape-command token-or-cmd s 'sh-escape))
        ((pathnamep token-or-cmd) (sh-escape (unix-namestring token-or-cmd) s))
        ((string= token-or-cmd "") (format s "\"\""))
        (t (uiop:escape-sh-token token-or-cmd s))))

(defun parse-username-from-id (output)
  "Where OUTPUT is the output of the id(1) command, extract the username."
  (multiple-value-bind (match groups)
      (re:scan-to-strings "^uid=[0-9]+\\(([^)]+)" output)
    (and match (elt groups 0))))

;; not DEFCONSFIG because a consfig is a system not a package
(defmacro defpackage-consfig (name &body forms)
  "Convenience wrapper around DEFPACKAGE for consfigs.
Adds recommended local nicknames for all the property and data source packages
that come with Consfigurator.  Either use this directly or use its macro
expansion as a starting point for your own DEFPACKAGE form for your consfig."
  (let ((forms (copy-tree forms))
        (local-nicknames
          (cons :local-nicknames
                (loop for package in (list-all-packages)
                      for name = (package-name package)
                      if (string-prefix-p "CONSFIGURATOR.PROPERTY." name)
                        collect (list (make-symbol (subseq name 23))
                                      (make-symbol name))
                      else if (string-prefix-p "CONSFIGURATOR.DATA." name)
                             collect (list (make-symbol (subseq name 14))
                                           (make-symbol name))))))
    (if-let ((form (loop for form on forms
                         when (and (listp (car form))
                                   (eql :local-nicknames (caar form)))
                           return form)))
      (rplaca form (nconc local-nicknames (cdar form)))
      (push local-nicknames forms))
    ;; Not much benefit to importing CONSFIGURATOR for the user as it only
    ;; needs to be done once, and some users will prefer to import qualified,
    ;; but we could also have:
    ;; (if-let ((form (loop for form on forms
    ;;                      when (and (listp (car form)) (eql :use (caar form)))
    ;;                        return form)))
    ;;   (rplaca form (list* :use '#:consfigurator (cdar form)))
    ;;   (push '(:use '#:cl '#:consfigurator) forms))
    `(defpackage ,name ,@forms)))

(defmacro lambda-ignoring-args (&body body)
  (multiple-value-bind (forms declarations) (parse-body body)
    (with-gensyms (ignore)
      `(lambda (&rest ,ignore)
         (declare (ignore ,ignore) ,@declarations)
         ,@forms))))

(defun parse-cidr (address-with-suffix)
  (destructuring-bind (address cidr)
      (split-string address-with-suffix :separator "/")
    (unless cidr
      (simple-program-error "~A is not in CIDR notation."
                            address-with-suffix))
    (values
     address
     (loop with cidr = (parse-integer cidr)
           with type = (if (or (> cidr 32) (find #\: address)) 6 4)
           with block-digits = (if (= type 4) 8 16)
           repeat (if (= type 4) 4 8)
           for digits = (min cidr block-digits)
           do (decf cidr digits)
           collect (parse-integer
                    (with-output-to-string (s)
                      (loop repeat digits do (princ #\1 s))
                      (loop repeat (- block-digits digits) do (princ #\0 s)))
                    :radix 2)
             into accum
           finally (return (if (= type 4)
                               (format nil "~{~D~^.~}" accum)
                               (with-output-to-string (s)
                                 (loop for blocks on accum
                                       if (> (car blocks) 0)
                                         do (format s "~X" (car blocks))
                                         and if (cdr blocks) do (princ #\: s)
                                               end
                                       else do (princ #\: s)
                                               (loop-finish)))))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-constant +alphanum+
    "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
    :test #'string=))

(defun random-alphanumeric (length)
  "Return a random alphanumeric string of length LENGTH."
  (aprog1 (make-string length)
    (loop with *random-state* = (make-random-state t)
          for i below length
          do (setf (char it i)
                   (char +alphanum+ (random #.(length +alphanum+)))))))

(defun mkfifo ()
  "Use mkfifo(3) to create a named pipe with a mkstemp(3)-like name."
  (let* ((dir (drop-trailing-slash (or (getenv "TMPDIR") "/tmp")))
         (dir-ls (run-program
                  `("env" "LANG=C" "ls" "-lnd" ,dir) :output :string))
         (prefix (strcat dir "/tmp.")))
    (unless (and (char= #\d (char dir-ls 0))
                 (char-equal #\t (char dir-ls 9))
                 (zerop (parse-integer (caddr (words dir-ls)))))
      (error "~A is not a root-owned dir with the sticky bit set." dir))
    (flet ((mktemp ()
             ;; We need to generate a temporary name.  We don't have to worry
             ;; about race conditions as mkfifo(3) will fail if the file
             ;; already exists.
             (loop with result = (make-string (+ 6 (length prefix)))
                     initially (setf (subseq result 0 (length prefix)) prefix)
                   for i from (length prefix) below (length result)
                   do (setf (char result i)
                            (char +alphanum+ (random #.(length +alphanum+))))
                   finally (return result)))
           (mkfifo (temp)
             (handler-case (nix:mkfifo temp #o600)
               (serious-condition (c)
                 (if (or (file-exists-p temp) (directory-exists-p temp))
                     nil
                     (signal c))))))
      (loop with *random-state* = (make-random-state t)
            repeat 3 for temp = (mktemp)
            when (mkfifo temp) return (pathname temp)))))

(defmacro with-mkfifos ((&rest mkfifos) &body forms)
  `(let ,(loop for mkfifo in mkfifos collect `(,mkfifo (mkfifo)))
     (unwind-protect (progn ,@forms)
       ,@(loop for mkfifo in mkfifos collect `(delete-file ,mkfifo)))))

(defun write-to-mkfifo (object fifo)
  (with-standard-io-syntax
    (write object :stream fifo) (terpri fifo) (finish-output fifo)))

(defun valid-hostname-p (string)
  "Test whether STRING looks like a valid hostname, as defined by RFCs 952 and
1123."
  (and
   (<= (length string) 253)
   (let ((parts (split-string string :separator ".")))
     (every (lambda (part)
              (and (<= (length part) 63)
                   (re:scan "^[a-zA-Z0-9][a-zA-Z0-9-]*$" part)))
            parts))))


;;;; Progress & debug printing

(defvar *consfigurator-debug-level* 0
  "Integer.  Higher values mean be more verbose during deploys.")

(defvar *inform-prefix* ";; ")

(defmacro with-indented-inform (&body forms)
  `(let ((*inform-prefix* (strcat *inform-prefix* "    ")))
     ,@forms))

(defun inform (level output &key strip-empty (fresh-line t))
  "Print something to the user during deploys."
  (unless (and (numberp level) (> level *consfigurator-debug-level*))
    (let ((lines (loop for line in (etypecase output
                                     (cons output)
                                     (string (lines output)))
                       ;; strip (first part of) prefix added by a remote Lisp
                       for stripped = (if (string-prefix-p ";; " line)
                                          (subseq line 3)
                                          line)
                       unless (and strip-empty (re:scan #?/\A\s*\z/ stripped))
                         collect stripped)))
      (when fresh-line
        (fresh-line)
        (princ *inform-prefix*))
      (princ (pop lines))
      (dolist (line lines)
        (fresh-line)
        (princ *inform-prefix*)
        (princ line)))))

(defun informat (level control-string &rest format-arguments)
  "Print something to the user during deploys using FORMAT.
Be sure to begin CONTROL-STRING with ~& unless you want to continue from
previous output."
  (if (string-prefix-p "~&" control-string)
      (inform level
              (apply #'format nil (subseq control-string 2) format-arguments)
              :fresh-line t)
      (inform level
              (apply #'format nil control-string format-arguments)
              :fresh-line nil)))


;;;; Version numbers

(defun compare-versions (x y &optional less-than-or-equal)
  (flet
      ((components (v)
         (etypecase v
           (number (list v))
           (string
            (loop with buf
                    = (make-array 0 :fill-pointer 0 :element-type 'character)
                  for c across v
                  if (digit-char-p c)
                    do (vector-push-extend c buf)
                  else if (and (char= c #\.) (plusp (fill-pointer buf)))
                         collect (parse-integer buf) into accum
                         and do (setf (fill-pointer buf) 0)
                  else do (loop-finish)
                  finally (return (if (plusp (fill-pointer buf))
                                      (nconc accum (list (parse-integer buf)))
                                      accum)))))))
    (setq x (components x) y (components y))
    (if less-than-or-equal
        (loop while (or x y) for a = (or (pop x) 0) and b = (or (pop y) 0)
              never (> a b)
              if (< a b) return t)
        (loop while (or x y) for a = (or (pop x) 0) and b = (or (pop y) 0)
              thereis (> b a)
              if (< b a) return nil))))

(defun version< (x y)
  (compare-versions x y))

(defun version<= (x y)
  (compare-versions x y t))

(defun version> (x y)
  (compare-versions y x))

(defun version>= (x y)
  (compare-versions y x t))


;;;; Encoding of strings to filenames

;; Encoding scheme based on one by Joey Hess -- File.configFileName in
;; propellor.  Try to avoid including non-alphanumerics other than '.' and '_'
;; in the filename, such that it both remains roughly human-readable and is
;; likely to be accepted by programs which don't treat filenames as opaque
;; (and interpret them with a charset sufficiently similar to Lisp's).

;; This implementation also assumes that the Lisp doing the decoding has the
;; same charset as the Lisp doing the encoding.

(defun string-to-filename (s)
  (apply #'concatenate 'string
         (loop for c
                 across (etypecase s (string s) (number (write-to-string s)))
               if (or (char= c #\.)
                      (alpha-char-p c)
                      (digit-char-p c))
                 collect (format nil "~C" c)
               else
                 collect (format nil "_~X_" (char-code c)))))

(defun filename-to-string (s)
  (loop with decoding
        with buffer
        with result
        for c across s
        do (cond
             ((and (char= c #\_) (not decoding))
              (setq decoding t))
             ((and (char= c #\_) decoding)
              (unless buffer (error "invalid encoding"))
              (push (code-char
                     (read-from-string
                      (coerce (list* #\# #\x (nreverse buffer)) 'string)))
                    result)
              (setq buffer nil
                    decoding nil))
             (decoding
              (push c buffer))
             (t
              (push c result)))
        finally (return (coerce (nreverse result) 'string))))


;;;; Forking utilities

;;; Use implementation-specific fork(2) wrapper, and never fork(2) itself, to
;;; allow the implementation to handle things like finaliser threads.  For all
;;; other syscalls/libc & POSIX macros like WIFEXITED, use CFFI, via Osicat
;;; when there's a wrapper available, for portability.

(defun fork ()
  ;; Normalise any other implementations such that we signal an error if
  ;; fork(2) returns -1, so caller doesn't have to check for that.
  #+sbcl (sb-posix:fork))

(defmacro forked-progn (child-pid child-form &body parent-forms)
  (with-gensyms (retval)
    `(progn
       #-(or sbcl) (error "Don't know how to safely fork(2) in this Lisp.")
       (mapc-open-output-streams
        #'force-output (list *debug-io* *terminal-io*
                             *standard-output* *error-output*))
       (let ((,retval (fork)))
         (if (zerop ,retval)
             ;; We leave it to the caller to appropriately call CLOSE or
             ;; CLEAR-INPUT on input streams shared with the parent, because
             ;; at least SBCL's CLEAR-INPUT clears the OS buffer as well as
             ;; Lisp's, potentially denying data to both sides of the fork.
             ,child-form
             (let ((,child-pid ,retval)) ,@parent-forms))))))

(define-condition skipped-properties () ()
  (:documentation
   "There were failed changes, but instead of aborting, that particular property
application was instead skipped over, either due to the semantics of a
property combinator, or because the user elected to skip the property in the
interactive debugger."))

(defmacro with-deployment-report (&body forms)
  (with-gensyms (failures)
    `(let* (,failures
            (result (handler-bind ((skipped-properties (lambda (c)
                                                         (declare (ignore c))
                                                         (setq ,failures t))))
                      ,@forms)))
       (inform
        t
        (cond
          ((eql :no-change result)
           "No changes were made.")
          (,failures
           "There were failures while attempting to apply some properties.")
          (t
           "Changes were made without any reported failures."))))))

(defmacro with-backtrace-and-exit-code (&body forms)
  (with-gensyms (failures)
    `(let* (,failures
            (result (handler-bind ((serious-condition
                                     (lambda (c)
                                       (trivial-backtrace:print-backtrace
                                        c :output *error-output*)
                                       (uiop:quit 1)))
                                   (skipped-properties (lambda (c)
                                                         (declare (ignore c))
                                                         (setq ,failures t))))
                      ,@forms)))
       (uiop:quit (cond ((eql :no-change result) 0)
                        (,failures               22)
                        (t                       23))))))

(defmacro exit-code-to-retval (exit &key on-failure)
  `(values
    nil
    (case ,exit
      (0                               :no-change)
      (22 (signal 'skipped-properties) nil)
      (23                              nil)
      (t                               ,on-failure))))

(defun posix-login-environment (&optional uid logname home)
  "Reset the environment after switching UID, or similar, in a :LISP connection.
Does not currently establish a PAM session."
  (let ((rootp (zerop (or uid (nix:geteuid))))
        (maybe-preserve '("TERM")))
    (when rootp
      (push "SSH_AUTH_SOCK" maybe-preserve))
    (let ((preserved (loop for var in maybe-preserve
                           for val = (getenv var)
                           when val collect var and collect val)))
      (clearenv)
      (loop for (var val) on preserved by #'cddr do (setf (getenv var) val)))
    (when logname
      (setf (getenv "USER") logname (getenv "LOGNAME") logname))
    (when home
      (setf (getenv "HOME") (drop-trailing-slash (unix-namestring home)))
      (uiop:chdir home))
    (setf (getenv "SHELL") "/bin/sh"
          (getenv "PATH")
          (if rootp
              "/usr/local/sbin:/usr/local/bin:/sbin:/bin:/usr/sbin:/usr/bin"
              "/usr/local/bin:/bin:/usr/bin"))))


;;;; System and libc calls which can fail

;;; Osicat has an implementation of this but it's not exported.  However, we
;;; are able to instantiate Osicat's POSIX-ERROR to simplify errno handling.

(defmacro define-error-retval-cfun
    ((&key (errno t) (failure-val -1)) &body defcfun-args)
  (let ((defun (etypecase (car defcfun-args)
                 (string
                  (translate-name-from-foreign (car defcfun-args) '*package*))
                 (list (cadar defcfun-args))))
        (cfun (etypecase (car defcfun-args)
                (string (car defcfun-args))
                (list (caar defcfun-args))))
        (failure-val-check
          (once-only (failure-val)
            `(cond ((numberp ,failure-val) (= ,failure-val result))
                   ((pointerp ,failure-val) (pointer-eq ,failure-val result))
                   (t (simple-program-error
                       "Don't know how to compare function return value with ~S."
                       ,failure-val))))))
    `(defun ,defun ,(loop for arg in (cddr defcfun-args) collect (car arg))
       ,@(and (eql errno :zero) '((nix:set-errno 0)))
       (let ((result (foreign-funcall
                      ,cfun
                      ,@(loop for arg in (cddr defcfun-args)
                              collect (cadr arg) collect (car arg))
                      ,(cadr defcfun-args))))
         (if ,(if (eql errno :zero)
                  `(and ,failure-val-check (not (zerop (nix:get-errno))))
                  failure-val-check)
             (nix:posix-error ,(and errno '(nix:get-errno)) nil ',defun)
             result)))))


;;;; Miscellaneous system functions

(define-error-retval-cfun () "clearenv" :int)

(define-error-retval-cfun () "chroot" :int (path :string))

(define-error-retval-cfun () "unshare" :int (flags :int))


;;;; Lisp data files

(defmacro with-lisp-data-file ((data file) &body forms)
  (with-gensyms (before)
    `(let* ((,before (and (file-exists-p ,file) (read-file-string ,file)))
            (,data (and ,before (plusp (length ,before))
                        (safe-read-from-string ,before))))
       (unwind-protect (progn ,@forms)
         (with-open-file
             (stream ,file :direction :output :if-exists :supersede)
           (with-standard-io-syntax
             (prin1 ,data stream)))))))


;;;; Streams

(defun stream->input-stream (stream)
  (etypecase stream
    (synonym-stream (stream->input-stream
                     (symbol-value (synonym-stream-symbol stream))))
    (two-way-stream (two-way-stream-input-stream stream))
    (stream (and (input-stream-p stream) stream))))

(defun mapc-open-input-streams (function streams)
  (dolist (stream streams streams)
    (when-let ((input-stream (stream->input-stream stream)))
      (when (open-stream-p input-stream)
        (funcall function input-stream)))))

(defun stream->output-stream (stream)
  (etypecase stream
    (synonym-stream (stream->output-stream
                     (symbol-value (synonym-stream-symbol stream))))
    (two-way-stream (two-way-stream-output-stream stream))
    (stream (and (output-stream-p stream) stream))))

(defun mapc-open-output-streams (function streams)
  (dolist (stream streams streams)
    (when-let ((output-stream (stream->output-stream stream)))
      (when (open-stream-p output-stream)
        (funcall function output-stream)))))
