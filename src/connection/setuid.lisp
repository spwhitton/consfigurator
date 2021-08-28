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

(in-package :consfigurator.connection.setuid)
(named-readtables:in-readtable :consfigurator)

(defun setuid (uid)
  #+sbcl      (sb-posix:setuid uid)
  #-(or sbcl) (foreign-funcall "setuid" :unsigned-int uid :int))

(defun setgid (gid)
  #+sbcl      (sb-posix:setgid gid)
  #-(or sbcl) (foreign-funcall "setgid" :unsigned-int uid :int))

(defun initgroups (user gid)
  (foreign-funcall "initgroups" :string user :unsigned-int gid :int))

(defclass setuid-connection (rehome-connection fork-connection) ())

(defmethod establish-connection ((type (eql :setuid)) remaining &key to)
  (unless (and (lisp-connection-p)
               (zerop (foreign-funcall "geteuid" :unsigned-int)))
    (error "~&SETUIDing requires a Lisp image running as root"))
  (informat 1 "~&SETUIDing to ~A" to)
  (multiple-value-bind (match groups)
      (re:scan-to-strings #?/uid=([0-9]+).+gid=([0-9]+)/ (run "id" to))
    (unless match
      (error "Could not determine UID and GID of ~A" to))
    (let* ((uid (parse-integer (elt groups 0)))
           (gid (parse-integer (elt groups 1)))
           (home
             ;; tilde expansion is POSIX
             (ensure-directory-pathname (stripln (run (strcat "echo ~" to)))))
           (xdg-cache-home
             (ensure-directory-pathname
              (stripln
               ;; su(1) is not POSIX but very likely to be present.  Note that
               ;; the -c argument here is to the user's login shell, not the
               ;; -c argument to su(1) on, e.g., FreeBSD.  So should be fairly
               ;; portable.
               (mrun "su" to "-c" "echo ${XDG_CACHE_HOME:-$HOME/.cache}")))))
      (continue-connection
       (make-instance
        'setuid-connection
        :datadir (merge-pathnames "consfigurator/data/" xdg-cache-home)
        :connattrs `(:remote-uid ,uid :remote-gid ,gid
                     :remote-user ,to :remote-home ,home
                     :XDG-CACHE-HOME ,xdg-cache-home))
       remaining))))

(defmethod post-fork ((connection setuid-connection))
  (let ((uid (connection-connattr connection :remote-uid))
        (gid (connection-connattr connection :remote-gid))
        (user (connection-connattr connection :remote-user)))
    (run-program (list "chown" "-R"
                       (format nil "~A:~A" uid gid)
                       (unix-namestring (slot-value connection 'datadir))))
    (posix-login-environment
     user (connection-connattr connection :remote-home))
    ;; We are privileged, so this sets the real, effective and saved IDs.
    (unless (zerop (setgid gid))
      (error "setgid(2) failed!"))
    (unless (zerop (initgroups user gid))
      (error "initgroups(3) failed!"))
    (unless (zerop (setuid uid))
      (error "setuid(2) failed!"))))
