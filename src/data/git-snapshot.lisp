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

(in-package :consfigurator.data.git-snapshot)
(named-readtables:in-readtable :consfigurator)

(defmethod register-data-source ((type (eql :git-snapshot))
                                 &key name repo depth branch)
  "Provide tarball snapshots of a branch of a local git repository.
Provides prerequisite data identified by \"--git-snapshot\", \"NAME\".

Rather than using git-bundle(1) or git-archive(1), we create a (possibly
shallow) clone and tar it up.  That way, it's still a git repo on the remote
side, but we don't require git to be installed on the remote side to get a
copy of the working tree over there."
  (when (data-source-providing-p "--git-snapshot" name)
    (simple-program-error
     "Another data source is providing git snapshots identified by ~S." name))
  (with-current-directory (repo)
    (unless (zerop (nth-value 2 (run-program '("git" "rev-parse" "--git-dir")
                                             :ignore-error-status t)))
      (missing-data-source "~A is not a git repository." repo)))
  (let* ((cached
           (get-highest-local-cached-prerequisite-data "--git-snapshot" name))
         (cached-commit (and cached (nth 1 (split-string (data-version cached)
                                                         :separator ".")))))
    (when cached
      (setf (data-mime cached) "application/gzip"))
    (labels ((latest-version (tip)
               (format nil "~A.~A" (get-universal-time) tip))
             (check (iden1 iden2)
               (and (string= iden1 "--git-snapshot")
                    (string= iden2 name)
                    (let ((tip (get-branch-tip repo branch)))
                      (if (and cached-commit (string= cached-commit tip))
                          (data-version cached)
                          (latest-version tip)))))
             (extract (&rest ignore)
               (declare (ignore ignore))
               (let* ((tip (get-branch-tip repo branch))
                      (version (latest-version tip))
                      (path (local-data-pathname
                             "--git-snapshot" name version)))
                 (if (and cached-commit (string= cached-commit tip))
                     cached
                     (progn
                       (ignore-errors
                        (mapc #'delete-file
                              (directory-files
                               (pathname-directory-pathname path))))
                       (make-snapshot name repo depth branch path)
                       (setq cached-commit tip
                             cached (make-instance 'file-data
                                                   :file path
                                                   :mime "application/gzip"
                                                   :iden1 "--git-snapshot"
                                                   :iden2 name
                                                   :version version)))))))
      (cons #'check #'extract))))

(defun make-snapshot (name repo depth branch output)
  (with-local-temporary-directory (dir)
    (let ((loc (ensure-directory-pathname (merge-pathnames name dir))))
      (run-program `("git" "clone"
                           "--no-hardlinks"
                           ,@(and depth `("--depth" ,(write-to-string depth)))
                           "--origin" "local"
                           ,@(and branch (list "--branch" branch))
                           ,(strcat "file://" (namestring repo))
                           ,(namestring loc)))
      (with-current-directory (loc)
        (run-program '("git" "remote" "rm" "local")))
      (delete-directory-tree (merge-pathnames ".git/refs/remotes/local/" loc)
                             :validate t :if-does-not-exist :ignore)
      (with-current-directory (dir)
        (run-program
         `("tar" "cfz" ,(namestring output) ,(namestring name)))))))

(defun get-branch-tip (repo branch)
  (with-current-directory (repo)
    (stripln
     (run-program `("git" "rev-parse" "--verify" ,(strcat branch "^{commit}"))
                  :output :string))))
