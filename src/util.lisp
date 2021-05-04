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

(defun noop (&rest args)
  "Accept any arguments and do nothing."
  (declare (ignore args))
  (values))

(defun lines (text)
  (split-string (stripln text) :separator '(#\Newline #\Return)))

(defun unlines (lines)
  (format nil "~{~A~%~}" lines))

(defmacro symbol-named (name symbol)
  `(and (symbolp ,symbol)
        (string= (symbol-name ',name) (symbol-name ,symbol))))

(defun normalise-system (system)
  (etypecase system
    (string system)
    (symbol (string-downcase
             (symbol-name system)))))

(defun memstring= (string list)
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

(defmacro define-simple-error (name &optional docstring)
  `(progn
     (define-condition ,name (simple-error) ()
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

(defun plist-to-cmd-args (plist &aux args)
  (doplist (k v plist args)
           (push (strcat "--" (string-downcase (symbol-name k)) "=" v) args)))

(defmacro with-local-temporary-directory ((dir) &body forms)
  "Execute FORMS with a local temporary directory's pathname in DIR.
Currently assumes GNU mktemp(1).

There is no WITH-REMOTE-TEMPORARY-DIRECTORY because POSIX doesn't have a way
to create temporary directories.  If you need a remote temporary directory,
one solution is to convert your property to a :LISP property."
  `(let ((,dir (ensure-directory-pathname
		(stripln
		 (run-program "umask 077; mktemp -d" :output :string)))))
     (unwind-protect (progn ,@forms)
       (delete-directory-tree ,dir :validate t))))

(defun pathname-file (pathname)
  "Like PATHNAME-NAME but include any file extension."
  (namestring
   (enough-pathname pathname (pathname-directory-pathname pathname))))

(defun drop-trailing-slash (namestring)
  (if (string-suffix-p namestring "/")
      (subseq namestring 0 (1- (length namestring)))
      namestring))

(defmacro quote-nonselfeval (x)
  (once-only (x)
    `(if (member (type-of ,x) '(cons symbol))
         `',,x ,x)))

(defmacro define-print-object-for-structlike (class)
  "Define an implementation of PRINT-OBJECT for objects which are simple
one-dimensional collections of values."
  `(defmethod print-object ((object ,class) stream)
     (if *print-readably*
         (format
          stream "#.~S"
          `(make-instance
            ',(type-of object)
            ;; Call CLASS-OF so that subclasses of CLASS are handled too.
            ,@(loop for slot in (closer-mop:class-slots (class-of object))
                    for initargs = (closer-mop:slot-definition-initargs slot)
                    and slot-name = (closer-mop:slot-definition-name slot)
                    when (slot-boundp object slot-name)
                      collect (car initargs)
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

(defun escape-sh-token (token &optional s)
  "Like UIOP:ESCAPE-SH-TOKEN, but also escape the empty string."
  (if (string= token "") (format s "\"\"") (uiop:escape-sh-token token s)))

(defun escape-sh-command (token &optional s)
  "Like UIOP:ESCAPE-SH-COMMAND, but also escape the empty string."
  (uiop:escape-command token s 'escape-sh-token))

(defun parse-username-from-id (output)
  "Where OUTPUT is the output of the id(1) command, extract the username."
  (multiple-value-bind (match groups)
      (re:scan-to-strings "^uid=[0-9]+\\(([^)]+)" output)
    (and match (elt groups 0))))


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

(defun version< (x y)
  (dpkg-version-compare x "<<" y))

(defun version> (x y)
  (dpkg-version-compare x ">>" y))

(defun version<= (x y)
  (dpkg-version-compare x "<=" y))

(defun version>= (x y)
  (dpkg-version-compare x ">=" y))

(defun dpkg-version-compare (x r y)
  (zerop (nth-value 2 (run-program `("dpkg" "--compare-versions"
                                            ,(etypecase x
                                               (string x)
                                               (number (format nil "~A" x)))
                                            ,r
                                            ,(etypecase y
                                               (string y)
                                               (number (format nil "~A" y))))
                                   :ignore-error-status t))))


;;;; Encoding of strings to filenames

;; Encoding scheme based on one by Joey Hess -- File.configFileName in
;; propellor.  Try to avoid including non-alphanumerics other than '.' and '_'
;; in the filename, such that it both remains roughly human-readable and is
;; likely to be accepted by programs which don't treat filenames as opaque
;; (and interpret them with a charset sufficiently similar to Lisp's).

;; This implementation also assumes that the Lisp doing the decoding has the
;; same charset as the Lisp doing the encoding.

(defun string->filename (s)
  (apply #'concatenate 'string
         (loop for c
                 across (etypecase s (string s) (number (write-to-string s)))
               if (or (char= c #\.)
                      (alpha-char-p c)
                      (digit-char-p c))
                 collect (format nil "~C" c)
               else
                 collect (format nil "_~X_" (char-code c)))))

(defun filename->string (s)
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

(define-condition in-child-process () ())

(defmacro unwind-protect-in-parent (protected &body cleanup)
  "Like UNWIND-PROTECT, but with a mechanism to cancel the execution of CLEANUP
in child processes resulting from calls to fork(2) during the execution of
PROTECTED.  This means that CLEANUP won't get executed on both sides of the
fork, but only in the parent.

For this to work, after fork(2), the child process must call
CANCEL-UNWIND-PROTECT-IN-PARENT-CLEANUP, which will affect all enclosing uses
of this macro."
  (with-gensyms (cancelled)
    `(let (,cancelled)
       (unwind-protect
            (handler-bind ((in-child-process
                             (lambda (c) (setq ,cancelled t) (signal c))))
              ,protected)
         (unless ,cancelled ,@cleanup)))))

(defun cancel-unwind-protect-in-parent-cleanup ()
  "Cancel the CLEANUP forms in all enclosing uses of UNWIND-PROTECT-IN-PARENT.
Should be called soon after fork(2) in child processes."
  (signal 'in-child-process))
