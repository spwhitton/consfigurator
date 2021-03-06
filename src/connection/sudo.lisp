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
(named-readtables:in-readtable :consfigurator)

;; Note that a password needed to sudo is technically not a piece of
;; prerequisite data required by a deployment, because it is not used in
;; deploying properties in the context of a connection chain which has already
;; been fully established.  Nevertheless, we can query sources of prerequisite
;; data to obtain passwords by following the conventions for having
;; prerequisite data sources provide them.

(defmethod preprocess-connection-args ((type (eql :sudo)) &key as (to "root"))
  (list :sudo
        :user to
        :password (and
                   as
                   (destructuring-bind (user host)
                       (split-string as :separator "@")
                     (get-data-protected-string
                      (strcat "--user-passwd--" host) user)))))

(defmethod establish-connection ((type (eql :sudo))
                                 remaining
                                 &key
                                   user
                                   password)
  (declare (ignore remaining))
  (informat 1 "~&Establishing sudo connection to ~A" user)
  (make-instance 'sudo-connection
                 :connattrs `(:remote-user ,user)
                 ;; we'll send the password followed by ^M, then the real
                 ;; stdin.  use CODE-CHAR in this way so that we can be sure
                 ;; ASCII ^M is what will get emitted.
                 :password (and password
                                (make-passphrase
                                 (strcat (passphrase password)
                                         (string (code-char 13)))))))

(defclass sudo-connection (shell-wrap-connection)
  ((password :initarg :password)))

(defmethod get-sudo-password ((connection sudo-connection))
  (let ((value (slot-value connection 'password)))
    (and value (passphrase value))))

(defmethod connection-shell-wrap ((connection sudo-connection) cmd)
  ;; Wrap in sh -c so that it is more likely we are either asked for a
  ;; password for all our commands or not asked for one for any.
  ;;
  ;; Preserve SSH_AUTH_SOCK for root to enable this sort of workflow: deploy
  ;; laptop using (:SUDO :SBCL) and then DEFHOST for laptop contains (DEPLOYS
  ;; ((:SSH :TO "root")) ...) to deploy a VM running on the laptop.
  ;;
  ;; This only works for sudoing to root because only the superuser can access
  ;; the socket (and was always able to, so we're not granting new access
  ;; which may be unwanted).
  (let ((user (connection-connattr connection :remote-user)))
    (format
     nil
"sudo -HkS --prompt=\"\" ~:[~;--preserve-env=SSH_AUTH_SOCK ~]--user=~A sh -c ~A"
     (string= user "root") user (escape-sh-token cmd))))

(defmethod connection-run ((c sudo-connection) cmd (input null))
  (call-next-method c cmd (get-sudo-password c)))

(defmethod connection-run ((c sudo-connection) cmd (input string))
  (call-next-method c cmd (strcat (get-sudo-password c) input)))

(defmethod connection-run ((connection sudo-connection) cmd (input stream))
  (call-next-method connection
                    cmd
                    (if-let ((password (get-sudo-password connection)))
                      (make-concatenated-stream
                       (if (subtypep (stream-element-type input) 'character)
                           (make-string-input-stream password)
                           (babel-streams:make-in-memory-input-stream
                            (babel:string-to-octets
                             password :encoding :UTF-8)
                            :element-type (stream-element-type input)))
                       input)
                      input)))
