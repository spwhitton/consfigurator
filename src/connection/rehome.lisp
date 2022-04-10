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

(in-package :consfigurator.connection.rehome)
(named-readtables:in-readtable :consfigurator)

(defclass rehome-connection ()
  ((datadir
    :type :string :initarg :datadir :reader datadir
    :documentation
    "Where Consfigurator would cache items of prerequisite data in the new HOME."))
  (:documentation
   "A connection which works by switching to a new HOME on the same host."))

(defmethod continue-connection
    :before ((connection rehome-connection) remaining)
    (upload-all-prerequisite-data connection))

(defmethod connection-upload ((connection rehome-connection) (data file-data))
  (with-slots (data-iden1 data-iden2 data-version) data
    (let ((inside (data-pathname
                   (datadir connection) data-iden1 data-iden2 data-version))
          (outside (remote-data-pathname data-iden1 data-iden2 data-version)))
      (mrun "mkdir" "-p" (pathname-directory-pathname inside))
      (if (remote-exists-p outside)
          (mrun "cp" outside inside)
          (let (done)
            (unwind-protect
                 (progn
                   (connection-upload (connection-parent connection) data)
                   (mrun "mv" outside inside)
                   (setq done t))
              (unless done (mrun "rm" "-f" outside))))))))

(defmethod connection-clear-data-cache
    ((connection rehome-connection) iden1 iden2)
  (with-slots (datadir) connection
    (delete-remote-trees (data-pathname (datadir connection) iden1 iden2))))

(defmethod get-remote-cached-prerequisite-data
    ((connection rehome-connection))
  (get-local-cached-prerequisite-data (datadir connection)))
