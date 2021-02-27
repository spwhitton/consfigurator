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

;; TODO Let's require the user pass :PASSWD or :NOPASSWD to indicate whether
;; we'll query prerequisite data sources and always try to send a password on
;; stdin, or never try to send one (and default to :NOPASSWD).  Will still
;; require an :AS parameter, which is redundant in a sense, but this way
;; avoids it being implicit that we're always going to be sending a password
;; on stdin, which latter has security implications (e.g. if a password is not
;; actually required then the password is going into random processes).

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
  (format t "Establishing sudo connection to ~A~%" user)
  (make-instance 'sudo-connection
		 :user user
		 ;; we'll send the password followed by ^M, then the real
		 ;; stdin.  use CODE-CHAR in this way so that we can be sure
		 ;; ASCII ^M is what will get emitted.
		 :password (strcat password (string (code-char 13)))))

(defclass sudo-connection (shell-wrap-connection)
  ((user
    :initarg :user)
   (password
    :initarg :password)))

(defmethod connection-shell-wrap ((connection sudo-connection) cmd)
  ;; wrap in sh -c so that it is more likely we are either asked for a
  ;; password for all our commands or not asked for one for any
  (format nil "sudo -HkS --prompt=\"\" --user=~A sh -c ~A"
	  (slot-value connection 'user) (escape-sh-token cmd)))

(defmethod connection-run ((c sudo-connection) cmd (input null))
  (call-next-method c cmd (slot-value c 'password)))

(defmethod connection-run ((c sudo-connection) cmd (input string))
  (call-next-method c cmd (strcat (slot-value c 'password) input)))

(defmethod connection-run ((connection sudo-connection) cmd (input stream))
  (call-next-method connection
		    cmd
		    (if-let ((password (slot-value connection 'password)))
		      (make-concatenated-stream
		       (if (subtypep (stream-element-type input) 'character)
			   (make-string-input-stream password)
			   (babel-streams:make-in-memory-input-stream
			    (babel:string-to-octets
			     password :encoding :UTF-8)
			    :element-type (stream-element-type input)))
		       input)
		      input)))

(defmethod connection-upload ((c sudo-connection) from to)
  (connection-run c #?"cp ${from} ${to}" nil))
