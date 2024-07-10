;;; Consfigurator -- Lisp declarative configuration management system

;;; Copyright (C) 2021-2022  David Bremner <david@tethera.net>
;;; Copyright (C) 2021-2022  Sean Whitton <spwhitton@spwhitton.name>

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

(in-package :consfigurator.property.user)
(named-readtables:in-readtable :consfigurator)

(defprop has-account :posix (username)
  "Ensure there is an account for USERNAME.
Note that this uses getent(1) and so is not strictly POSIX-compatible."
  (:desc #?"Account for ${username}")
  (:check
   (user-exists username))
  (:apply
   (assert-remote-euid-root)
   (mrun "useradd" "-m" username)))

(defprop %has-uid-gid :posix (username uid gid)
  (:check
   (and (= uid (parse-integer (passwd-field 2 username)))
        (= gid (parse-integer (passwd-field 3 username)))
        (= gid (parse-integer (group-entry 2 username)))))
  (:apply
   (let* ((gid-str (write-to-string gid))
          (uid-str (write-to-string uid))
          (uid+gid (format nil "~d:~d" uid gid))
          (home (passwd-field 5 username)))
     (mrun "groupmod" "--gid" gid-str username)
     (mrun "usermod" "--uid" uid-str "--gid" gid-str username)
     (mrun "chown" "-R" uid+gid home))))

(defproplist has-account-with-uid :posix (username uid &key (gid uid))
  "Ensure there is an account for USERNAME with uid UID.
Also ensure the group named USERNAME has gid GID, USERNAME's primary group is
that group, and ~USERNAME and its contents are owned by UID:GID."
  (:hostattrs (os:required 'os:debianlike))
  (:desc #?"${username} has uid ${uid} gid ${gid}")
  (has-account username)
  (%has-uid-gid username uid gid))

(defprop group-exists :posix (groupname)
  "Ensure there is a group GROUPNAME.
Note that this uses getent(1) and so is not strictly POSIX-compatible."
  (:desc #?"Group ${groupname} exists")
  (:check
   (zerop (mrun :for-exit "getent" "group" groupname)))
  (:apply
   (assert-remote-euid-root)
   (mrun "groupadd" groupname)))

(defun get-secondary-groups (username)
  (etypecase (get-hostattrs-car :os)
    (os:freebsd
     (cdr (words (stripln (run "id" "-Gn" username)))))
    (os:debianlike
     (cdddr (words (stripln (run "groups" username)))))))

(defprop has-groups :posix (username &rest groups)
  "Ensure that USERNAME is a member of secondary groups GROUPS."
  (:desc (format nil "~A in group~P ~A" username (length groups)
                 (format nil "~{~A~^,~}" groups)))
  (:check (subsetp groups (get-secondary-groups username) :test #'string=))
  (:apply
   (assert-remote-euid-root)
   (typecase (get-hostattrs-car :os)
     (os:freebsd
      (mrun "pw" "usermod" username
            "-G" (format nil "~{~A~^,~}"
                         (union (get-secondary-groups username) groups
                                :test #'string=))))
     (t
      (mrun "usermod" "-a" "-G" (format nil "~{~A~^,~}" groups) username)))))

(defparameter *desktop-groups*
  '("audio" "cdrom" "dip" "floppy" "video" "plugdev" "netdev" "scanner"
    "bluetooth" "debian-tor" "lpadmin")
  "See the debconf template passwd/user-default-groups for package user-setup.")

(defprop has-desktop-groups :posix (username)
  "Add user to the secondary groups to which the OS installer normally adds the
default account it creates.  Skips over groups which do not exist yet, pending
the installation of other software."
  (:desc #?"${username} is in standard desktop groups")
  (:hostattrs (os:required 'os:debianlike))
  (:apply
   (let ((existing-groups
           (loop for line in (lines (read-remote-file "/etc/group"))
                 collect (car (split-string line :separator ":")))))
     (apply #'has-groups username (loop for group in *desktop-groups*
                                        when (memstr= group existing-groups)
                                          collect group)))))

(defprop has-login-shell :posix (username shell)
  "Ensures that USERNAME has login shell SHELL.
Note that this uses getent(1) and so is not strictly POSIX-compatible."
  (:desc #?"${username} has login shell ${shell}")
  (:check
   (string= (passwd-field 6 username) shell))
  (:apply
   (file:contains-lines "/etc/shells" shell)
   (mrun "chsh" "-s" shell username)))

(defprop has-enabled-password :posix
    (username &key (initial-password "changeme"))
  "Ensures that it is possible to login as USERNAME; if this requires enabling
the account's password, also set it to INITIAL-PASSWORD.
The main purpose of this property is to ensure that in a freshly installed
system it will be possible to log in.  The password should usually be changed
to something which is not stored in plain text in your consfig right after,
and then this property will do nothing."
  (:desc #?"${username} has an enabled password")
  (:check
   (declare (ignore initial-password))
   (string= "P" (cadr (split-string (run "passwd" "-S" username)))))
  (:apply
   (mrun :input (format nil "~A:~A" username initial-password) "chpasswd")))

(defprop has-locked-password :posix (username)
  "Ensure that USERNAME cannot login via a password."
  (:desc #?"${username} has a locked password")
  (:hostattrs (os:required 'os:debianlike))
  (:check
   (assert-remote-euid-root)
   (string= "L" (cadr (split-string (run "passwd" "-S" username)))))
  (:apply
   (assert-remote-euid-root)
   (mrun "passwd" "--lock" username)))

(defun %getent-entry (n name-or-id &optional (database "passwd"))
  "Get the nth entry in the getent(1) output for NAME-OR-ID in DATABASE."
  (let ((u (etypecase name-or-id
             (string name-or-id)
             (number (write-to-string name-or-id)))))
    (nth n (split-string (stripln (mrun "getent" database u))
                         :separator ":"))))

(defun passwd-field (n username-or-uid)
  "Get the nth entry in the getent(1) output for USERNAME-OR-UID.
Note that getent(1) is not specified in POSIX so use of this function makes
properties not strictly POSIX-compatible."
  (%getent-entry n username-or-uid "passwd"))

(defun group-entry (n groupname-or-gid)
  "Get the nth entry in the getent(1) output for GROUPNAME-OR-GID.
Note that getent(1) is not specified in POSIX so use of this function makes
properties not strictly POSIX-compatible."
  (%getent-entry n groupname-or-gid "group"))

(defun user-exists (username)
  (zerop (mrun :for-exit "getent" "passwd" username)))

(defun user-info (username-or-uid)
  "Return passwd database entry for USERNAME-OR-UID as an alist.

Falls back to getent(1), which is not specified in POSIX, so use of this
function makes properties not strictly POSIX-compatible."
  ;; getpwnam(3) and getpwuid(3) can fail to load the required NSS modules if
  ;; we have chrooted or similar.  In that case, it appears as though the user
  ;; does not exist.  So fall back to getent(1).
  (or (and (lisp-connection-p) (osicat:user-info username-or-uid))
      (aand (runlines "getent" "passwd" (aetypecase username-or-uid
                                          (string it)
                                          (number (write-to-string it))))
            (destructuring-bind (name password uid gid &rest rest)
                (split-string (car it) :separator '(#\:))
              (declare (ignore password))
              (list* (cons :name name)
                     (cons :user-id (parse-integer uid))
                     (cons :group-id (parse-integer gid))
                     (pairlis '(:gecos :home :shell) rest))))))
