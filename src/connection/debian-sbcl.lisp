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


;; (handler-bind ((consfigurator:missing-data-source
;;                 #'consfigurator:skip-data-source))
;;   ...)


(defmethod establish-connection ((type (eql :debian-sbcl)) remaining &key)
  ;; any connection type which starts up a Lisp connection is going to want to
  ;; do something like what this loop does, so just make it a core function?
  ;; (loop for system in (slot-value (slot-value *host* :hostattrs) :systems)
  ;; 	do (push (cons "lisp-system" system) (getf *host* :data)))

  (unless (= 0 (nth-value 1 (run "which" "sbcl" "2>/dev/null"
				 "||" "apt-get" "-y" "install" "sbcl")))
    (error "Could not get sbcl installed on the remote host"))
  (upload-all-prerequisite-data)

  ;; I think we want a function in data.lisp which returns a LOAD form which
  ;; will load a given lisp system out of a local cache.  After calling
  ;; upload-all-prerequisite-data we can call that from here to get a form
  ;; suitable for feeding to remote sbcl.  Slight layering violation, and only
  ;; to be called by connections, not properties.  But better than exposing
  ;; get-remote-data-cache-dir.

  ;; PROGRAM is (load "~/.cache/...") (deploy :local host properties)
  ;; (multiple-value-bind ()
  ;;     (run :input program "sbcl"
  ;; 	   "--noinform"
  ;; 	   "--noprint"
  ;; 	   "--disable-debugger"
  ;; 	   "--no-sysinit"
  ;; 	   "--no-user-init"))
  ;; relay its output and signal something if it exits nonzero

  )
