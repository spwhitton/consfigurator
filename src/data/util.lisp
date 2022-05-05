;;; Consfigurator -- Lisp declarative configuration management system

;;; Copyright (C) 2022  David Bremner <david@tethera.net>
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

(in-package :consfigurator.data.util)
(named-readtables:in-readtable :consfigurator)

(defun literal-data-pathname (base-path iden1 iden2 &key type)
  "Generate a path from BASE-PATH, IDEN1 and IDEN2 by concatentation,
optionally adding extension TYPE.

No escaping of special characters is done, but extra '/' characters between
pathname components are removed.

The intended use case is to map IDEN1 and IDEN2 to files in a user-maintained
hierarchy under BASE-PATH.  In particular IDEN2 and (if prefixed by '_') IDEN1
may contain '/' characters to map into multiple levels of directory."
  (let ((base-dir (uiop:parse-unix-namestring base-path :ensure-directory t)))
    (unless (uiop:directory-pathname-p base-dir)
      (simple-program-error "~A does not specify a directory" base-dir))
    (merge-pathnames
     (uiop:relativize-pathname-directory
      (uiop:parse-unix-namestring iden2 :type type))
     (merge-pathnames
      (uiop:relativize-pathname-directory
       (ensure-directory-pathname iden1))
      base-dir))))

(defun gpg (args &key input output)
  "Run gnupg, taking homedir from *DATA-SOURCE-GNUPGHOME* if set.

INPUT and OUTPUT have the same meaning as for RUN-PROGRAM, except that OUTPUT
defaults to :STRING.  The default return value is thus the output from gnupg,
as a string."
  (run-program
   `("gpg"
     ,@(and *data-source-gnupghome*
            (list "--homedir" (namestring *data-source-gnupghome*)))
     ,@args)
   :input  input
   :output (or output :string)))

(defun gpg-file-as-string (location)
  "Decrypt the contents of a gpg encrypted file at LOCATION, return as a
string."
  (handler-case
      (gpg (list "--decrypt" (unix-namestring location)))
    (subprocess-error (error)
      (missing-data-source "While attempt to decrypt ~A, gpg exited with ~A"
			   location (uiop:subprocess-error-code error)))))
