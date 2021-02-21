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

(defmethod preprocess-connection-args ((type (eql :sudo)) &key as to)
  (list :sudo
	:user to
	:password (and
		   as
		   (destructuring-bind (user host)
		       (split-string as :separator "@")
		     (get-data-string (strcat "-user-passwd--" host) user)))))

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

(defmethod connection-run ((connection sudo-connection) cmd &optional input))

(defmethod connection-readfile ((connection sudo-connection) path))

(defmethod connection-writefile ((connection sudo-connection) path contents))

(defmethod connection-upload ((connection sudo-connection) from to))

;; always wrap in sh -c so that we can be sure that a password will be
;; consistently asked for or not asked for.  and we don't make a single string
;; with the whole command to run, but pass the command and its args
;;
;; so, ``sudo -HkS --user=USER sh -c ARGS`` and prepend password\n to INPUT
