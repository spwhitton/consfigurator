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

;; With sudo -S, we must ensure that sudo's stdin is a pipe, not a file,
;; because otherwise the program sudo invokes may rewind(stdin) and read the
;; password, intentionally or otherwise.  And UIOP:RUN-PROGRAM empties input
;; streams into temporary files, so there is the potential for this to happen
;; when using :SUDO to apply properties to localhost.  Other connection types
;; might work similarly.
;;
;; The simplest way to handle this would be to just put 'cat |' at the
;; beginning of the shell command we construct, but that relies on cat(1) not
;; calling rewind(stdin) either.  So we write the password input out to a
;; temporary file ourselves, and use cat(1) to concatenate that file with the
;; actual input.

(defclass sudo-connection (shell-wrap-connection)
  ((password-file :initarg :password-file)))

(defmethod establish-connection ((type (eql :sudo))
                                 remaining
                                 &key
                                   user
                                   password)
  (declare (ignore remaining))
  (informat 1 "~&Establishing sudo connection to ~A" user)
  (make-instance
   'sudo-connection
   :connattrs `(:remote-user ,user)
   :password-file (and password
                       (aprog1 (mktemp)
                         ;; We'll send the password followed by ^M, then the
                         ;; real stdin.  Use CODE-CHAR in this way so that we
                         ;; can be sure ASCII ^M is what will get emitted.
                         (writefile it (strcat (passphrase password)
                                               (string (code-char 13)))
                                    :mode #o600)))))

(defmethod connection-teardown :after ((connection sudo-connection))
  (when-let ((file (slot-value connection 'password-file)))
    (delete-remote-trees file)))

(defmethod connection-run ((connection sudo-connection) cmd input)
  (let* ((file (slot-value connection 'password-file))
         (user (connection-connattr connection :remote-user))
         (prefix (if file
                     (format nil "cat ~A - | sudo -HkS --prompt=\"\""
                             (sh-escape file))
                     "sudo -Hkn")))
    ;; Wrap in sh -c so that it is more likely we are either asked for a
    ;; password for all our commands or not asked for one for any.
    ;;
    ;; Preserve SSH_AUTH_SOCK for root to enable this sort of workflow: deploy
    ;; laptop using (:SUDO :SBCL) and then DEFHOST for laptop contains
    ;; (DEPLOYS ((:SSH :TO "root")) ...) to deploy a VM running on the laptop.
    ;;
    ;; This only works for sudoing to root because only the superuser can
    ;; access the socket (and was always able to, so we're not granting new
    ;; access which may be unwanted).
    (mrun :may-fail :input input
          (format nil
                  "~A ~:[~;--preserve-env=SSH_AUTH_SOCK ~]--user=~A sh -c ~A"
                  prefix (string= user "root") user (sh-escape cmd)))))
