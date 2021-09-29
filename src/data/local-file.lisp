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

(in-package :consfigurator.data.local-file)
(named-readtables:in-readtable :consfigurator)

(defmethod register-data-source
    ((type (eql :local-file)) &key file version iden1 iden2)
  "Provide the contents of a single local file on the machine running the root
Lisp.  Register this data source more than once to provide multiple files.
The version of the data provided is either VERSION or the file's last
modification time."
  (unless (file-exists-p file)
    (missing-data-source "~A does not exist." file))
  (cons (lambda (iden1* iden2*)
          (and (string= iden1 iden1*) (string= iden2 iden2*)
               (file-exists-p file)
               (or version (file-write-date file))))
        (lambda (iden1* iden2*)
          (and (string= iden1 iden1*) (string= iden2 iden2*)
               (file-exists-p file)
               (make-instance 'file-data
                              :file file
                              :iden1 iden1 :iden2 iden2
                              :version (or version (file-write-date file)))))))
