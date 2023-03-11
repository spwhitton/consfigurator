;;; Consfigurator -- Lisp declarative configuration management system

;;; Copyright (C) 2022  David Bremner <david@tethera.net>

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

(in-package :consfigurator.data.pass)
(named-readtables:in-readtable :consfigurator)

(defmethod register-data-source ((type (eql :pass))
                                 &key (location "~/.password-store"))
  "Provide the contents of a pass(1) store on the machine running the root
Lisp.  Register this data source multiple times to provide multiple stores.

LOCATION specifies the root of the password store.

LOCATION, IDEN1, and IDEN2 are concatenated to locate a file in the password
store.

For retrieving user account passwords, IDEN1 can be a valid hostname or
'--user-passwd--HOST' where HOST is a valid hostname, and IDEN2 the username.
Otherwise, IDEN1 should begin with '_' (see the 'Prerequisite Data' section of
the Consfigurator user's manual).  In the latter case, if the concatenated
path does not exist in the password store then the search is tried again after
dropping the '_'.  This means that while user consfigs should always prefix
any IDEN1 that is not a valid hostname or of the form '--user-passwd--HOST'
with '_', existing pass(1) entries do not need to be renamed.  Other forms for
IDEN1 are not supported by this data source."
  (let ((base-path (ensure-directory-pathname location)))
    (unless (directory-exists-p base-path)
      (missing-data-source
       "~A does not exist, or is not a directory." base-path))
    (labels
        ((%gpg-file-p (iden1 iden2)
           (file-exists-p
            (literal-data-pathname base-path iden1 iden2 :type "gpg")))
         (%make-path (iden1 iden2)
           (acond
             ((strip-prefix "--user-passwd--" iden1)
              (and (valid-hostname-p it) (%gpg-file-p it iden2)))
             ((strip-prefix "_" iden1)
              (or (%gpg-file-p iden1 iden2) (%gpg-file-p it iden2)))
             (t
              (and (valid-hostname-p iden1) (%gpg-file-p iden1 iden2)))))
         (check (iden1 iden2)
           (when-let ((file-path (%make-path iden1 iden2)))
             (file-write-date file-path)))
         (extract (iden1 iden2)
           (when-let ((file-path (%make-path iden1 iden2)))
             (make-instance 'string-data
                            :string (stripln (gpg-file-as-string file-path))
                            :iden1 iden1
                            :iden2 iden2
                            :version (file-write-date file-path)))))
      (cons #'check #'extract))))
