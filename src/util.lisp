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

(defun ordinary-ll-variable-names (ll)
  (loop for arg in ll
	for arg* = (ensure-car arg)
	do (assert-ordinary-ll-member arg)
	unless (char= #\& (char (symbol-name arg*) 0))
	  collect arg*))

(defmacro define-simple-error (name &optional docstring)
  `(progn
     (define-condition ,name (simple-error) ()
       ,@(and docstring `((:documentation ,docstring))))
     (defun ,name (message &rest args)
       ,@(and docstring `(,docstring))
       (error ',name :format-control message :format-arguments args))))

(defmacro form-beginning-with (sym form)
  `(and (listp ,form) (eq ',sym (car ,form))))

(defun strip-declarations (forms)
  (loop while (form-beginning-with declare (car forms))
	do (pop forms)
	finally (return forms)))


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
