(in-package :consfigurator.core)

(defvar *connection* (make-connection)
  "Object representing the currently active connection.
Deployments bind this variable, but its global value should be regarded as a
constant.  This constant value is a connection representing the root
deployment: deploying properties to the machine Lisp is running on.")

;; private struct; access this functionality using primitive properties
(defstruct connection type run readfile writefile upload teardown)

;; exported struct, to let user code construct properties
(defstruct property type desc hostattrs check apply unapply)

;; standard way to write properties is to use this macro, or one of the
;; property combinator functions
(defmacro defprop (name type args &body forms)
  (let (slots)
    (when (stringp (car forms))
      (setf (getf slots :desc) (pop forms)))
    (loop for form in forms
	  if (keywordp (car form))
	  do (setf (getf slots (car form)) (cdr form)))
    (loop for kw in '(:check :apply :unapply)
	  do (let ((slot (getf slots kw)))
	       (setf (getf slots kw)
		     (if slot
			 `(lambda ,args ,@slot)
			 `(lambda (&rest args)
			    (declare (ignore args))
			    (values))))))
    `(progn
       (defvar ,name (make-property) ,(getf slots :desc))
       (setf (property-type ,name) ,type)
       ,@(loop for slot in '(:desc :hostattrs :check :apply :unapply)
	       ;; if (getf slots slot)
	       collect
	       `(setf (,(intern
			 (concatenate 'string "PROPERTY-" (symbol-name slot)))
			,name)
		      ,(getf slots slot))))))
