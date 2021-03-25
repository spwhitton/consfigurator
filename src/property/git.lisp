;;; Consfigurator -- Lisp declarative configuration management system

;;; Copyright (C) 2020-2021  Sean Whitton <spwhitton@spwhitton.name>

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
   (delete-remote-tree dest)
   (file:directory-exists directory)
   (with-remote-current-directory (directory)
     (mrun :input (get-data-stream "--git-snapshot" snapshot-name)
           "tar" "xfz" "-")))
  (:unapply
   (declare (ignore replace))
   (delete-remote-tree dest)))
