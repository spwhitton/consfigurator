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
