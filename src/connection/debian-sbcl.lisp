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

(in-package :consfigurator.connection.debian-sbcl)

(defmethod establish-connection ((type (eql :debian-sbcl)) remaining &key)
  (loop for system in (slot-value (slot-value *host* :hostattrs) :systems)
	do (push (cons "lisp-system" system) (getf *host* :data)))
  (unless (= 0 (nth-value 1 (run "which" "sbcl" "2>/dev/null"
				 "||" "apt-get" "-y" "install" "sbcl")))
    (error "Could not get sbcl installed on the remote host"))
  (upload-all-prerequisite-data)
  ;; now we generate and upload a Lisp file which will load all the
  ;; lisp-system prerequisite data we just uploaded and call (deploy :local
  ;; host properties), execute `sbcl --script <path to tiny file>`, and relay
  ;; its output, signalling an error if it exits nonzero
  )
