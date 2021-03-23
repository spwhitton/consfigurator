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

(in-package :consfigurator.property.file)
(named-readtables:in-readtable :consfigurator)

(defun map-file-lines (file function)
  "Apply FUNCTION to the lines of FILE.  Safe to use in a :POSIX property.

For efficiency, a :LISP property might want to use streams, but there's no
point in doing that here because WRITEFILE is synchronous."
  (let* ((orig-lines (lines (readfile file)))
         (new-lines (funcall function orig-lines)))
    (if (equal orig-lines new-lines)
        :no-change
        (writefile file (unlines new-lines)))))

(defprop has-content :posix (path content)
  "Ensure there is a file at PATH whose content is CONTENT.
CONTENT can be a list of lines or a single string."
  (declare (indent 1))
  (:apply (writefile path (etypecase content
                            (cons (unlines content))
                            (string (format nil "~A~&" content))))))

(defprop contains-lines :posix (path lines)
  "Ensure there is a file at PATH containing each of LINES once."
  (:apply
   (let ((new-lines (copy-list (ensure-cons lines)))
         (existing-lines (lines (readfile path))))
     (dolist (existing-line existing-lines)
       (deletef new-lines existing-line :test #'string=))
     (writefile path (unlines (nconc existing-lines new-lines))))))

(defprop has-mode :posix (path mode)
  "Ensure that a file has a particular numeric mode."
  (:desc (format nil "~A has mode ~O" path mode))
  (:apply
   (mrun (format nil "chmod ~O ~A" mode path))))

(defprop does-not-exist :posix (&rest paths)
  "Ensure that files do not exist."
  (:desc (if (cdr paths)
             #?"@{paths} do not exist"
             #?"${(car paths)} does not exist"))
  (:apply (mrun "rm" "-f" paths)))

(defprop data-uploaded :posix (iden1 iden2 destination)
  (:hostattrs
   (declare (ignore destination))
   (require-data iden1 iden2))
  (:apply
   (writefile destination (get-data-stream iden1 iden2))))

(defprop host-data-uploaded :posix (destination)
  (:hostattrs
   (require-data (get-hostname) destination))
  (:apply
   (data-uploaded (get-hostname) destination destination)))

(defprop secret-uploaded :posix (iden1 iden2 destination)
  (:hostattrs
   (declare (ignore destination))
   (require-data iden1 iden2))
  (:apply
   (writefile destination (get-data-stream iden1 iden2) :mode #o600)))

(defprop host-secret-uploaded :posix (destination)
  (:hostattrs
   (require-data (get-hostname) destination))
  (:apply
   (secret-uploaded (get-hostname) destination destination)))

(defprop regex-replaced-lines :posix (file regex replace)
  "Like s/REGEX/REPLACE/ on the lines of FILE.
Uses CL-PPCRE:REGEX-REPLACE, which see for the syntax of REPLACE."
  (:apply
   (map-file-lines
    file
    (lambda (lines)
      (mapcar (lambda (line) (re:regex-replace regex line replace)) lines)))))

(defprop directory-exists :posix (dir)
  "Ensure that a directory and its parents exists."
  (:desc (strcat dir " exists"))
  (:apply
   (mrun "mkdir" "-p" dir)))
