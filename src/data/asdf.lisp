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

(in-package :consfigurator.data.asdf)
(named-readtables:in-readtable :consfigurator)

(defmethod register-data-source ((type (eql :asdf)) &key)
  (cons #'asdf-data-source-check #'get-path-to-concatenated-system))

(defun asdf-data-source-check (iden1 system)
  (let ((system (and (string= iden1 "--lisp-system")
                     (asdf:find-system system nil))))
    (and system (system-version system))))

(defun get-path-to-concatenated-system (iden1 system)
  "Try to concatenate all the source code for SYSTEM, store it somewhere and
return the filename."
  (let ((op 'asdf:concatenate-source-op)
        (co (asdf:find-component system nil)))
    (asdf:operate op co)
    (make-instance 'file-data :file (asdf:output-file op co)
                              :mime "text/plain"
                              :iden1 iden1
                              :iden2 system
                              :version (system-version co))))

(defun system-version (system)
  (reduce #'max
          (mapcar #'file-write-date
                  (asdf:input-files 'asdf:concatenate-source-op system))))
