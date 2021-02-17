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

(in-package :consfigurator.data.pgp)

;; Simple PGP-encrypted file source of prerequisite data

;; We provide an implementation of REGISTER-DATA-SOURCE and functions for the
;; user to call at the REPL to add pieces of data, see what's there, etc.  (a
;; prerequisite data source which was some sort of external file-generating or
;; secrets storage database might not provide any functions for the REPL).
