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
;;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

(in-package :consfigurator.connection.setuid)
(named-readtables:in-readtable :consfigurator)

(defclass setuid-connection (rehome-connection fork-connection) ())

(defmethod establish-connection ((type (eql :setuid)) remaining &key user)
  (unless (and (lisp-connection-p) (zerop (nix:geteuid)))
    (error "~&SETUIDing requires a Lisp image running as root"))
  (informat 1 "~&SETUIDing to ~A" user)
  (let* ((ent
           (or (user:user-info user)
               (failed-change "~&Could not look up user info for ~A." user)))
	 (xdg-cache-home
           (ensure-directory-pathname
            (stripln
             ;; su(1) is not POSIX but very likely to be present.  Note that
             ;; the -c argument here is to the user's login shell, not the -c
             ;; argument to su(1) on, e.g., FreeBSD.  So should be fairly
             ;; portable.
             (mrun "su" (cdr (assoc :name ent))
		   "-c" "echo ${XDG_CACHE_HOME:-$HOME/.cache}"))))
         (cache (merge-pathnames "consfigurator/" xdg-cache-home))
         (datadir (merge-pathnames "data/" cache)))
    (dolist (dir (list xdg-cache-home cache datadir))
      (unless (directory-exists-p dir)
        (nix:chown (ensure-directories-exist dir)
                   (cdr (assoc :user-id ent)) (cdr (assoc :group-id ent)))))
    (continue-connection
     (make-instance
      'setuid-connection
      :rehome-datadir datadir
      :connattrs `(:remote-uid ,(cdr (assoc :user-id ent))
                   :remote-gid ,(cdr (assoc :group-id ent))
                   :remote-user ,(cdr (assoc :name ent))
                   :remote-home ,(ensure-directory-pathname
                                  (cdr (assoc :home ent)))
                   :consfigurator-cache ,cache
                   :XDG_CACHE_HOME ,xdg-cache-home))
     remaining)))

(defmethod post-fork ((connection setuid-connection))
  (let ((uid (connection-connattr connection :remote-uid))
        (gid (connection-connattr connection :remote-gid))
        (user (connection-connattr connection :remote-user)))
    (run-program
     (list "chown" "-R"
           (format nil "~A:~A" uid gid)
           (unix-namestring (slot-value connection 'rehome-datadir))))
    (posix-login-environment
     uid user (connection-connattr connection :remote-home))
    ;; We are privileged, so this sets the real, effective and saved IDs.
    (nix:setgid gid) (nix:initgroups user gid) (nix:setuid uid)))

(defmethod propagate-connattr
    ((type (eql :no-services)) connattr (connection setuid-connection))
  connattr)
