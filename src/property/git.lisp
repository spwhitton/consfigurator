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

(in-package :consfigurator.property.git)
(named-readtables:in-readtable :consfigurator)

(defproplist installed :posix ()
  "Ensures that git(1) is installed."
  (:desc "Git installed")
  (os:etypecase
    (debianlike (apt:installed "git"))))

(defprop snapshot-extracted :posix
    (directory snapshot-name
               &key replace
               &aux (dest
                     (merge-pathnames snapshot-name
                                      (ensure-directory-pathname directory))))
  "Extract a tarball as produced by DATA:GIT-SNAPSHOT under DIRECTORY.
If REPLACE, delete and replace the snapshot (or anything else) that already
exists at DIRECTORY/SNAPSHOT-NAME.  This is useful to ensure the latest
available version of the snapshot is present on the remote system."
  ;; TODO Keyword argument to replace only if a newer version of the
  ;; prerequisite data is available.
  (:desc (declare (ignore replace dest))
         #?"git snapshot ${snapshot-name} extracted")
  (:hostattrs (declare (ignore replace dest))
              (require-data "--git-snapshot" snapshot-name))
  (:check (and (not replace) (remote-exists-p dest)))
  (:apply
   (declare (ignore replace))
   (delete-remote-trees dest)
   (file:directory-exists directory)
   (with-remote-current-directory (directory)
     (mrun :input (get-data-stream "--git-snapshot" snapshot-name)
           "tar" "xfz" "-")))
  (:unapply
   (declare (ignore replace))
   (delete-remote-trees dest)))

(defprop %cloned :posix (url dest branch
                             &aux (dest (ensure-directory-pathname dest)))
  (:check
   (declare (ignore branch))
   (let ((config (merge-pathnames ".git/config" dest)))
     (and (remote-exists-p config)
          (string= url (car (runlines "git" "config" "--file" config
                                      "remote.origin.url"))))))
  (:apply
   (delete-remote-trees dest)
   (file:containing-directory-exists dest)
   (run "git" "clone" url dest)
   (with-remote-current-directory (dest)
     (when branch
       (mrun "git" "checkout" branch))
     ;; Do this in case this repo is to be served via HTTP, though note that
     ;; we don't set up the hook to do this upon update here.
     (mrun "git" "update-server-info"))))

(defproplist cloned :posix (url dest &optional branch)
  "Clone git repo available at URL to DEST.
If the directory already exists and contains anything but a git repo cloned
from URL, recursively delete it first.  If BRANCH, check out that branch."
  (:desc #?"${url} cloned to ${dest}")
  (installed)
  (%cloned url dest branch))

(defprop %pulled :posix (dest &aux (dest (ensure-directory-pathname dest)))
  (:apply
   (with-change-if-changes-file-content
       ((merge-pathnames ".git/FETCH_HEAD" dest))
     (with-remote-current-directory (dest)
       (mrun "git" "pull")
       (mrun "git" "update-server-info")))))

(defproplist pulled :posix (url dest &optional branch)
  "Like GIT:CLONED, but also 'git pull' each time this property is applied."
  (:desc #?"${url} pulled to ${dest}")
  (installed)
  (%cloned url dest branch)
  (%pulled dest))

(defprop repo-configured :posix (repo &rest pairs)
  (:desc
   (format nil "git repo at ~S has configuration ~{~A=~A~^, ~}" repo pairs))
  (:check (loop for (k v) on pairs by #'cddr
                always (string= v (stripln
                                   (run :may-fail "git" "-C" repo "config" k)))))
  (:apply (loop for (k v) on pairs by #'cddr
                do (mrun "git" "-C" repo "config" k v))))
