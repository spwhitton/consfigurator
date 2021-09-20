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

(in-package :consfigurator.connection.fork)
(named-readtables:in-readtable :consfigurator)

(defclass fork-connection (local-connection) ())

(defgeneric post-fork (connection)
  (:documentation
   "Code to execute after forking/reinvoking but before calling CONTINUE-DEPLOY*.
Must not start up any threads."))

(defmethod continue-connection ((connection fork-connection) remaining)
  (upload-all-prerequisite-data connection)
  (eval-in-grandchild `(post-fork ,connection)
      `(continue-deploy* ,connection ',remaining) (out err exit)
    (when-let ((lines (lines out)))
      (inform t lines))
    (return-exit
     exit
     :on-failure (failed-change
                  "~&Fork connection child failed; stderr was ~%~%~A" err))))
