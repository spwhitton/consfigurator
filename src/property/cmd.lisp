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

(in-package :consfigurator.property.cmd)
(named-readtables:in-readtable :consfigurator)

;; The name of this property comes from the idea that we might want to add a
;; property in this package to run a user-supplied shell script.
(defprop single :posix (&rest args)
  "A property which can be applied by running a single shell command.  ARGS is
either a single string specifying a shell-escaped command, or number of
strings which will be shell-escaped and then concatenated.

(Note that bypassing the shell can only be done within a :LISP property.)

Keyword argument :ENV is a plist of environment variables to be set when
running the command, using env(1)."
  (:desc (loop for arg in args
               if (stringp arg)
                 collect (escape-sh-token arg) into accum
               else collect (prin1-to-string arg) into accum
               finally (return (format nil "~{~A~^ ~}" accum))))
  (:apply (apply #'mrun args)))
