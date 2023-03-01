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

(in-package :consfigurator)

(named-readtables:defreadtable :consfigurator
  (:merge :standard)
  (:dispatch-macro-char #\# #\? #'cl-interpol:interpol-reader)
  (:dispatch-macro-char #\# #\> #'cl-heredoc:read-heredoc))

(named-readtables:defreadtable :consfigurator.without-read-eval
  (:merge :consfigurator)
  (:dispatch-macro-char #\# #\. (constantly nil)))
