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

(in-package :consfigurator.data.asdf)
(named-readtables:in-readtable :consfigurator)

(defmethod register-data-source ((type (eql :asdf)) &key)
  (cons #'asdf-data-source-check #'get-path-to-system-tarball))

(defun asdf-data-source-check (iden1 system)
  (let ((system (and (string= iden1 "--lisp-system")
                     (asdf:find-system system nil))))
    (and system (system-version-files system))))

(defun get-path-to-system-tarball (iden1 system)
  (let* ((tarball (merge-pathnames
                   (strcat "consfigurator/systems/" system ".tar.gz")
                   (uiop:xdg-cache-home)))
         (tarball-write-date
           (and (file-exists-p tarball) (file-write-date tarball))))
    (multiple-value-bind (version files) (system-version-files system)
      (if (and tarball-write-date (>= tarball-write-date version))
          (setq version tarball-write-date)
          (let* ((dir (asdf:system-source-directory system))
                 (relative
                   (loop for file in files
                         if (subpathp file dir)
                           collect (unix-namestring
                                    (enough-pathname file dir))
                         else
                           do (error "~A is not a subpath of ~A." file dir))))
            (run-program
             (list* "tar" "-C" (unix-namestring dir)
                    "-czf" (unix-namestring (ensure-directories-exist tarball))
                    relative))))
      (make-instance 'file-data :file tarball :mime "application/gzip"
                                :iden1 iden1 :iden2 system :version version))))

(defun system-version-files (system)
  (let* ((system (asdf:find-system system))
         (name (asdf:component-name system))
         (file (asdf:system-source-file system))
         (written (file-write-date file)))
    (unless (string= (pathname-name file) name)
      (error "Cannot upload secondary systems directly."))
    (labels ((recurse (component)
               (let ((pathname (asdf:component-pathname component))
                     (rest (and (compute-applicable-methods
                                 #'asdf:component-children (list component))
                                (mapcan #'recurse
                                        (asdf:component-children component)))))
                 (if (and pathname (file-exists-p pathname))
                     (progn (maxf written (file-write-date pathname))
                            (cons pathname rest))
                     rest))))
      ;; We include secondary systems because otherwise, for systems using the
      ;; package-inferred-system extension, we could end up uploading a huge
      ;; number of tarballs.  We need to ensure SYSTEM is loaded so that all
      ;; the secondary systems are known to ASDF.
      (asdf:load-system system)
      (let ((files
              (nconc
               (recurse system)
               (loop for other in (asdf:registered-systems)
                     for other* = (asdf:find-system other)
                     when (and (not (eql system other*))
                               (string= name (asdf:primary-system-name other*)))
                       nconc (recurse other*)))))
        (values written (cons file files))))))
