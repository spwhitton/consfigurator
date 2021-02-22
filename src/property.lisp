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

;;;; Properties

;; Properties are not stored as CLOS objects (or structs) in value cells
;; because they are immutable -- see "Attempting to work with anonymous
;; properties or connection types" in the docs.  A determined user could of
;; course edit the symbol plist entries and/or function cell, but we want to
;; make it a bit more difficult for someone who hasn't read that part of the
;; docs to accidentally violate immutability.

(defun setprop (sym type &key args desc hostattrs check apply unapply)
  ;; use non-keyword keys to avoid clashes with other packages
  (when type
    (setf (get sym 'type) type))
  (when args
    (setf (get sym 'args) args))
  (when desc
    (setf (get sym 'desc) desc))
  (when hostattrs
    (setf (get sym 'hostattrs) hostattrs))
  (when check
    (setf (get sym 'check) check))
  (when apply
    (setf (get sym 'apply) apply)
    (eval `(defun ,sym ()))
    (setf (symbol-function sym)
	  (if check
	      (lambda (&rest args)
		(unless (apply check args)
		  (apply apply args)))
	      apply)))
  (when unapply
    (setf (get sym 'unapply) unapply))
  sym)

(defun proptype (prop)
  (get prop 'type))

(defun propapptype (propapp)
  (get (car propapp) 'type))

(defun collapse-types (&rest lists)
  (if (member :posix (flatten lists)) :posix :lisp))

(defun propdesc (prop)
  (get prop 'desc))

(defun propargs (prop)
  (get prop 'args))

(defun propattrs (prop &rest args)
  (apply (get prop 'hostattrs #'noop) args))

(defun propappattrs (propapp)
  (apply #'propattrs propapp))

(defun propcheck (prop &rest args)
  (apply (get prop 'check #'noop) args))

(defun propappcheck (propapp)
  (apply #'propcheck propapp))

(defun propappapply (propapp)
  (apply (symbol-function (car propapp)) (cdr propapp)))

(defun propunapply (prop &rest args)
  (apply (get prop 'unapply #'noop) args))

(defun propappunapply (propapp)
  (apply #'propunapply propapp))

;;; standard way to write properties is to use one of these two macros

(defmacro defprop (name type args &body forms)
  (let ((slots (list :args (list 'quote args))))
    (when (stringp (car forms))
      (setf (getf slots :desc) (pop forms)))
    (loop for form in forms
	  if (keywordp (car form))
	  do (setf (getf slots (car form)) (cdr form)))
    (loop for kw in '(:hostattrs :check :apply :unapply)
	  do (if-let ((slot (getf slots kw)))
	       (setf (getf slots kw)
		     ;; inside this lambda we could do some checking of, e.g.,
		     ;; whether we are :lisp but this connection is
		     ;; posix-connection.  possibly a condition with a restart
		     ;; which allows skipping over this property
		     `(lambda ,args ,@slot))))
    `(setprop ',name ,type ,@slots)))

(defmacro defproplist (name type args &body propspec)
  "Define a property which applies a property application specification.
PROPSPEC is an unevaluated property application specification."
  (with-gensyms (props)
    `(let ((,props (props ,propspec)))
       (defprop ,name ,type ,args
	 (:hostattrs
	  (eval-propspec-hostattrs ,props))
	 (:apply
	  (eval-propspec ,props))))))


;;;; hostattrs in property subroutines

(defun get-hostattrs (k)
  "Retrieve the list of static informational attributes of type KEY.

Called by property :HOSTATTRS, :APPLY and :UNAPPLY subroutines."
  (getf (slot-value *host* 'hostattrs) k))

(defun push-hostattrs (k &rest vs)
  "Push new static informational attributes VS of type KEY.

Called by property :HOSTATTRS subroutines."
  (loop for v in vs
	do (push v (getf (slot-value *host* 'hostattrs) k))))

(defun require-data (iden1 iden2)
  "Wrapper around PUSH-HOSTATTRS to indicate that a piece of prerequisite data
is needed to deploy a property.

Called by property :HOSTATTRS subroutines."
  (push-hostattrs :data (cons iden1 iden2)))

(defun get-hostname ()
  "Get the hostname of the host to which properties are being applied.

Called by property subroutines."
  (car (get-hostattrs :hostname)))
