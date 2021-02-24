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

(defprop has-content :posix (path lines)
  "Ensure there is a file at PATH whose lines are the elements of LINES."
  (:apply (writefile path (unlines lines))))

(defprop contains-lines :posix (path lines)
  "Ensure there is a file at PATH containing each of LINES once."
  (:apply
   (let ((new-lines (copy-list lines))
	 (existing-lines (lines (readfile path))))
     (dolist (existing-line existing-lines)
       (deletef new-lines existing-line :test #'string=))
     (writefile path (unlines (nconc existing-lines new-lines))))))

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
