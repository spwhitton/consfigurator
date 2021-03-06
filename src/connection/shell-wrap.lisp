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

(in-package :consfigurator.connection.shell-wrap)
(named-readtables:in-readtable :interpol-syntax)

(defclass shell-wrap-connection (posix-connection) ())

(defgeneric connection-shell-wrap (connection cmd))

(defmethod connection-run ((c shell-wrap-connection) cmd input)
  (mrun :may-fail :input input (connection-shell-wrap c cmd)))

(defmethod connection-readfile ((c shell-wrap-connection) path)
  (multiple-value-bind (out exit)
      (let ((path (escape-sh-token path)))
	(connection-run c #?"test -r ${path} && cat ${path}" nil))
    (if (zerop exit) out (error "File ~S not readable" path))))

(defmethod connection-writefile ((conn shell-wrap-connection)
				 path
				 content
				 mode)
  (with-remote-temporary-file
      (temp :connection conn :directory (pathname-directory-pathname path))
    ;; TODO do we want a CONNECTION-ERROR condition to tidy this up?
    (multiple-value-bind (out exit)
	(connection-run conn
			(format nil "chmod ~O ~A" mode
				(escape-sh-token temp))
			nil)
      (unless (zerop exit) (error "Failed to chmod ~A: ~A" temp out)))
    (multiple-value-bind (out exit)
	(connection-run conn #?"cat >${temp}" content)
      (unless (zerop exit) (error "Failed to write ~A: ~A" temp out)))
    (multiple-value-bind (out exit)
	(connection-run
	 conn
	 #?"mv ${(escape-sh-token temp)} ${(escape-sh-token path)}"
	 nil)
      (unless (zerop exit) (error "Failed to write ~A: ~A" temp out)))))
