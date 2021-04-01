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

(in-package :consfigurator.connection.setuid)
(named-readtables:in-readtable :consfigurator)

(defun setuid (uid)
  #+sbcl      (sb-posix:setuid uid)
  #-(or sbcl) (foreign-funcall "setuid" :unsigned-int uid :int))

(defun setgid (gid)
  #+sbcl      (sb-posix:setgid gid)
  #-(or sbcl) (foreign-funcall "setgid" :unsigned-int uid :int))

(defclass setuid-connection (rehome-connection fork-connection)
  ((uid :type :integer :initarg :uid)
   (gid :type :integer :initarg :gid)
   (home :type :string :initarg :home)))

(defmethod establish-connection ((type (eql :setuid)) remaining &key to)
  (unless (and (lisp-connection-p) (zerop (foreign-funcall "geteuid" :int)))
    (error "~&SETUIDing requires a Lisp image running as root"))
  (informat 1 "~&SETUIDing to ~A" to)
  (re:register-groups-bind ((#'parse-integer uid gid))
      (#?/uid=([0-9]+).+gid=([0-9]+)/ (mrun "id" to))
    (let ((home (user:passwd-entry 5 uid))
          (datadir
            (ensure-directory-pathname
             (stripln
              (mrun
               "su" to "-c"
               "echo ${XDG_CACHE_HOME:-$HOME/.cache}/consfigurator/data/")))))
      (continue-connection
       (make-instance
        'setuid-connection :uid uid :gid gid :datadir datadir :home home)
       remaining))))

(defmethod post-fork ((connection setuid-connection))
  ;; TODO Set up the new environment more systematically.  Perhaps look at how
  ;; runuser(1) uses PAM to do this.
  (with-slots (uid gid home datadir) connection
    (run-program (list "chown" "-R"
                       (format nil "~A:~A" uid gid)
                       (unix-namestring datadir)))
    (unless (zerop (setgid gid))
      (error "setgid(2) failed!"))
    (unless (zerop (setuid uid))
      (error "setuid(2) failed!"))
    (setf (getenv "HOME") home)
    (uiop:chdir home)))
