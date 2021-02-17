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

(defprop file-has-content :posix (path lines)
  "Ensure there is a file at PATH whose lines are the elements of LINES."
  (:apply (writefile path (unlines lines))))

(defprop file-contains-lines :posix (path lines)
  "Ensure there is a file at PATH containing each of LINES."
  (:apply (let ((new-lines (copy-list lines))
		(existing-lines (lines (readfile path))))
	    (loop for existing-line in existing-lines
		  do (setq new-lines (delete existing-line new-lines)))
	    (writefile path (unlines
			     (nconc existing-lines new-lines))))))
