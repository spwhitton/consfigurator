(in-package :consfigurator.util)

(defun shellcmd (&rest args)
  "Return a single string representing the UNIX shell command whose arguments
are each element of ARGS.  Escapes as necessary.  Use `:input' and `:output'
for input and output redirection."
  (format nil "~{~A~^ ~}" (loop for arg in args
				if (eq arg :input)
				collect "<"
				else if (eq arg :output)
				collect ">"
				else collect (shell-escape arg))))

;; Based on Emacs' `shell-quote-argument'.  A standard alternative is to wrap
;; in single quotation marks and replace every intervening single quotation
;; mark with '"'"', but this has the disadvantage of escaping arguments which
;; don't need escaping, which may reduce readability when debugging.
(defun shell-escape (arg)
  "Return a string which represents, in POSIX shell syntax, a single argument
with contents ARG.

Escapes characters which would be interpreted by the shell."
  (cl-ppcre:regex-replace-all "\\n"
			      (cl-ppcre:regex-replace-all "[^-0-9a-zA-Z_./\n]"
							  arg
							  "\\\\\\&")
			      "'\\n'"))

(defmacro concat (&rest args)
  "Abbreviation for concatenating strings."
  `(concatenate 'string ,@args))

(defun noop (&rest args)
  "Accept any arguments and do nothing."
  (declare (ignore args))
  (values))

(defun lines (text)
  (uiop:split-string (uiop:stripln text) :separator '(#\Newline)))

(defun unlines (lines)
  (format nil "~{~A~%~}" lines))

(defmacro symbol-named (name symbol)
  `(and (symbolp ,symbol)
	(string= (symbol-name ',name) (symbol-name ,symbol))))


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
	 (loop for c across s
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
		      (coerce (cons #\# (cons #\x (nreverse buffer)))
			      'string)))
		    result)
	      (setq buffer nil
		    decoding nil))
	     (decoding
	      (push c buffer))
	     (t
	      (push c result)))
	finally (return (coerce (nreverse result) 'string))))
