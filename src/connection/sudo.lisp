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

(in-package :consfigurator.connection.sudo)
(named-readtables:in-readtable :interpol-syntax)

;; Note that a password needed to sudo is technically not a piece of
;; prerequisite data required by a deployment, because it is not used in
;; deploying properties in the context of a connection chain which has already
;; been fully established.  Nevertheless, we can query sources of prerequisite
;; data to obtain passwords by following the conventions for having
;; prerequisite data sources provide them.

;; Passing :as implies using a password, not passing it means assume NOPASSWD.
;; We only support querying prerequisite data sources for passwords.

;; Be aware that if any connection types which start up remote Lisp images
;; occur before a :sudo entry in your connection chain, ESTABLISH-CONNECTION
;; will need to inform the newly-started remote Lisp image of any sudo
;; passwords needed for establishing the remaining hops.  Depending on how the
;; connection type feeds instructions to the remote Lisp image, this may
;; involve writing your sudo password to a file under ~/.cache on the machine
;; which runs the remote Lisp image.  At least :debian-sbcl avoids this by
;; sending your password in on stdin.

(defmethod preprocess-connection-args ((type (eql :sudo)) &key as (to "root"))
  (list :sudo
	:user to
	:password (and
		   as
		   (destructuring-bind (user host)
		       (split-string as :separator "@")
		     (get-data-string (strcat "--user-passwd--" host) user)))))

(defmethod establish-connection ((type (eql :sudo))
				 remaining
				 &key
				   user
				   password)
  (declare (ignore remaining))
  (make-instance 'sudo-connection :user user :password password))

(defclass sudo-connection (posix-connection)
  ((user
    :initarg :user)
   (password
    :initarg :password)))

(defun sudocmd (connection &rest args)
  ;; wrap in sh -c so that it is more likely we are either asked for a
  ;; password for all our commands or not asked for one for any
  (format nil "sudo -HkS --prompt=\"\" --user=~A sh -c ~A"
	  (slot-value connection 'user)
	  (escape-sh-token
	   (if (cdr args) (escape-sh-command args) (car args)))))

(defmethod connection-run ((c sudo-connection) cmd &optional input)
  ;; send the password followed by ^M, then the real stdin.  use CODE-CHAR in
  ;; this way so that we can be sure ASCII ^M is what will get emitted.
  (let* ((input-stream
	   (typecase input
	     (stream input)
	     (string (make-string-input-stream input))))
	 (password (slot-value c 'password))
	 (password-stream (and password
			       (make-string-input-stream
				(format nil "~A~A" password (code-char 13)))))
	 (new-input (cond
		      ((and password input)
		       (make-concatenated-stream password-stream input-stream))
		      (password
		       password-stream)
		      (input
		       input-stream)
		      (t
		       nil))))
    (multiple-value-bind (out err exit-code)
	(run :may-fail :input new-input (sudocmd c cmd))
      (values (strcat err out) exit-code))))

(defmethod connection-readfile ((c sudo-connection) path)
  (multiple-value-bind (out exit-code)
      (connection-run
       c
       (format nil "test -r ~A && cat ~:*~A" (escape-sh-token path)))
    (if (= 0 exit-code)
	out
	(error "File ~S not readable" path))))

(defmethod connection-writefile ((c sudo-connection) path contents)
  (connection-run c #?"cat >$(path)" contents))

(defmethod connection-upload ((c sudo-connection) from to)
  (run (sudocmd c "cp" from to)))
