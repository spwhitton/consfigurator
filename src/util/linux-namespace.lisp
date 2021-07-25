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

(in-package :consfigurator.util.linux-namespace)
(named-readtables:in-readtable :consfigurator)

#+linux
(defun get-userns-owner (fd)
  (with-foreign-object (owner 'uid_t)
    (if (minusp
         (foreign-funcall
          "ioctl" :int fd :unsigned-long +NS_GET_OWNER_UID+ :pointer owner
                  :int))
        (error "Couldn't determine owner of target userns.")
        (mem-ref owner 'uid_t))))

(defun setgroups-p ()
  "In a Lisp-type connection, do we have the ability to use setgroups(2)?"
  (and #-linux (zerop (nix:geteuid))
       #+linux (capability-p :cap-effective +CAP-SETGID+)
       #+linux (string= "allow"
                        (stripln
                         (read-file-string "/proc/thread-self/setgroups")))))
