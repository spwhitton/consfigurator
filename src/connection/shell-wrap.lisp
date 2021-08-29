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
(named-readtables:in-readtable :consfigurator)

(defclass shell-wrap-connection (posix-connection) ())

(defgeneric connection-shell-wrap (connection cmd))

(defmethod connection-run ((c shell-wrap-connection) cmd input)
  (mrun :may-fail :input input (connection-shell-wrap c cmd)))

(defun %readfile (c path &optional delete)
  (multiple-value-bind (out exit)
      (let* ((path (escape-sh-token path))
             (base #?"test -r ${path} && cat ${path}")
             (cmd (if delete (strcat base #?"&& rm ${path}") base)))
        (connection-run c cmd nil))
    (if (zerop exit)
        out
        (error "Could not read~:[~; and/or remove~] ~S" delete path))))

(defmethod connection-readfile ((c shell-wrap-connection) path)
  (%readfile c path))

(defmethod connection-readfile-and-remove ((c shell-wrap-connection) path)
  (%readfile c path t))

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
         #?"mv ${(escape-sh-token temp)} ${(escape-sh-token (unix-namestring path))}"
         nil)
      (unless (zerop exit) (error "Failed to write ~A: ~A" path out)))))
