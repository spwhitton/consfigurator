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
  (apply #'mrun :may-fail :input input
         (ensure-cons (connection-shell-wrap c cmd))))

(defun %readfile (c path &optional delete)
  (multiple-value-bind (out exit)
      (let* ((path (sh-escape path))
             (base #?"test -r ${path} && cat ${path}")
             (cmd (if delete (strcat base #?" && rm ${path}") base)))
        (connection-run c cmd nil))
    (if (zerop exit)
        out
        (error "Could not read~:[~; and/or remove~] ~S" delete path))))

(defmethod connection-read-file ((c shell-wrap-connection) path)
  (%readfile c path))

(defmethod connection-read-and-remove-file ((c shell-wrap-connection) path)
  (%readfile c path t))

(defmethod connection-write-file ((conn shell-wrap-connection)
                                  path
                                  content
                                  mode)
  (let* ((mkstemp (mkstemp-cmd
                   (merge-pathnames "tmp.XXXXXX"
                                    (pathname-directory-pathname path))))
         (cmd (sh-script-to-single-line
               (format nil "set -e
                            tmpf=$(~A)
                            trap \"rm -f '$tmpf'\" EXIT HUP KILL TERM INT
                            chmod ~O \"$tmpf\"
                            cat >\"$tmpf\"
                            mv \"$tmpf\" ~A" mkstemp mode (sh-escape path)))))
    (multiple-value-bind (out exit) (connection-run conn cmd content)
      (unless (zerop exit)
        (error "Failed to write ~A: ~A" path out)))))
