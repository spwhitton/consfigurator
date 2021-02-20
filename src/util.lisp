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

(defun noop (&rest args)
  "Accept any arguments and do nothing."
  (declare (ignore args))
  (values))

(defun lines (text)
  (split-string (stripln text) :separator '(#\Newline)))

(defun unlines (lines)
  (format nil "~{~A~%~}" lines))

(defmacro symbol-named (name symbol)
  `(and (symbolp ,symbol)
	(string= (symbol-name ',name) (symbol-name ,symbol))))


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
  (= 0 (nth-value 2 (run-program (list "dpkg" "--compare-versions" x r y)
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
