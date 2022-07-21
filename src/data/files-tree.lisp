;;; Consfigurator -- Lisp declarative configuration management system

;;; Copyright (C) 2021  David Bremner <david@tethera.net>

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

(in-package :consfigurator.data.files-tree)
(named-readtables:in-readtable :consfigurator)

(defmethod register-data-source ((type (eql :files-tree)) &key location)
  "Provide the contents of a local directory on the machine running the root
Lisp.  Register this data source multiple times to provide multiple trees.

LOCATION is either a designator for a pathname representing the root of the
tree of files or a symbol which designates an ASDF package where the tree is
contained in the subdirectory 'data/'.

LOCATION, IDEN1 and IDEN2 are concatenated to locate files.  Thus, IDEN1
specifies a (possibly nested) subdirectory under LOCATION and IDEN2 a relative
path within that subdirectory.

Special characters in IDEN1 and IDEN2 are not encoded.  This means that each
character in IDEN1 and IDEN2 must be permitted in filenames on this system,
and that any slashes in IDEN1 and IDEN2 will probably act as path separators.

For convenience IDEN1 and IDEN2 may be passed as absolute and will be
converted to relative paths.  The usual cases of IDEN1 as a hostname, IDEN1 as
an underscore-prefixed identifier, and IDEN2 an an absolute or relative path
are all supported."
  (let ((base-path (if (symbolp location)
                        (asdf:system-relative-pathname location "data/")
                        (ensure-directory-pathname location))))
    (unless (directory-exists-p base-path)
      (missing-data-source
       "~A does not exist, or is not a directory." base-path))
    (labels ((check (iden1 iden2)
               (let ((file-path (literal-data-pathname base-path iden1 iden2)))
                 (and (file-exists-p file-path)
                      (file-write-date file-path))))
             (extract (iden1 iden2)
               (let ((file-path (literal-data-pathname base-path iden1 iden2)))
                 (and (file-exists-p file-path)
                      (make-instance 'file-data
                                     :file file-path
                                     :iden1 iden1
                                     :iden2 iden2
                                     :version (file-write-date file-path))))))
      (cons #'check #'extract))))
