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

(in-package :consfigurator.data.asdf)
(named-readtables:in-readtable :consfigurator)

;; could we have both :asdf-monolithic and :asdf-something_else where in the
;; latter we filter out the names of systems already known to be available on
;; the remote side, so those don't need to be uploaded?  for example, the
;; :sbcl connection type can try to install them with apt on the remote side,
;; then ask asdf for a concatenated source for everything excluding those.  if
;; asdf can't be asked to do that, maybe we can ask it to produce one file per
;; system, and then we eliminate those we don't want and concatenate the
;; result ourselves.  maybe we can create a fake system object based on the
;; real one, remove some deps from it according to a known mapping of systems
;; to Debian package names, then ask asdf to concatenate that system

(defmethod register-data-source ((type (eql :asdf)) &key)
  (cons #'asdf-data-source-check #'get-path-to-concatenated-system))

(defun asdf-data-source-check (iden1 system)
  (when (and (string= iden1 "--lisp-system")
	     (asdf:find-system system nil))
    (get-universal-time)))

(defun get-path-to-concatenated-system (iden1 system)
  "Try to concatenate all the source code for SYSTEM, store it somewhere and
return the filename."
  (let ((cache-dir (ensure-directory-pathname
		    (strcat (or (getenv "XDG_CACHE_HOME")
				(strcat (getenv "HOME") "/.cache"))
			    "/consfigurator/systems")))
	(op 'asdf:monolithic-concatenate-source-op)
	(co (asdf:find-component system nil)))
    (ensure-directories-exist cache-dir)
    (asdf:initialize-output-translations `(:output-translations
					   (t ,cache-dir)
					   :disable-cache
					   :ignore-inherited-configuration))
    (asdf:operate op co)
    (make-instance 'file-data :file (asdf:output-file op co)
			      :mime "text/plain"
			      :iden1 iden1
			      :iden2 system
			      :version (get-universal-time))))
