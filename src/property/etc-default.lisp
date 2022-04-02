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

(in-package :consfigurator.property.etc-default)
(named-readtables:in-readtable :consfigurator)

(defpropspec contains :posix
    (file &rest pairs &aux (file* (merge-pathnames file #P"/etc/default/")))
  "Where PAIRS is a list of even length of alternating keys and values, set each
of these keys and values in /etc/default/FILE."
  (:desc (format nil "~A has ~{~A=~S~^, ~}" file* pairs))
  `(file:contains-conf-shell ,file* ,@pairs))
