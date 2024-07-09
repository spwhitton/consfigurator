;;; Consfigurator -- Lisp declarative configuration management system

;;; Copyright (C) 2024  Sean Whitton <spwhitton@spwhitton.name>

;;; This file is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3, or (at your option)
;;; any later version.

;;; This file is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.

;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

(in-package :consfigurator.property.rc.conf)
(named-readtables:in-readtable :consfigurator)

;;;; FreeBSD's rc.conf(5) files

(defun sysrc (args &optional (file "/etc/rc.conf" file-supplied-p))
  (with-change-if-changes-file-content (file)
    (mrun "sysrc" (and file-supplied-p `("-f" ,file)) args)))

(defprop contains :posix (&rest pairs)
  (:desc (format nil "rc.conf has ~{~A=~S~^, ~}" pairs))
  (:hostattrs (os:required 'os:freebsd))
  (:apply (sysrc (loop for (k v) on pairs by #'cddr collect #?"${k}=${v}"))))

(defprop file-contains :posix (file &rest pairs)
  (:desc (format nil "~A has ~{~A=~S~^, ~}" file pairs))
  (:hostattrs (os:required 'os:freebsd))
  (:apply (sysrc (loop for (k v) on pairs by #'cddr collect #?"${k}=${v}")
                 file)))

(defprop ws-list-contains :posix (key &rest values)
  (:desc (format nil "rc.conf ~A has ~{~S~^, ~}" key values))
  (:hostattrs (os:required 'os:freebsd))
  (:apply (sysrc (loop for value in values collect #?"${key}+=${value}"))))

(defprop file-ws-list-contains :posix (file key &rest values)
  (:desc (format nil "~A ~A has ~{~S~^, ~}" file key values))
  (:hostattrs (os:required 'os:freebsd))
  (:apply (sysrc (loop for value in values collect #?"${key}+=${value}")
                 file)))

(defprop ws-list-lacks :posix (key &rest values)
  (:desc (format nil "rc.conf ~A lacks ~{~S~^, ~}" key values))
  (:hostattrs (os:required 'os:freebsd))
  (:apply (sysrc (loop for value in values collect #?"${key}-=${value}"))))

(defprop file-ws-list-lacks :posix (file key &rest values)
  (:desc (format nil "~A ~A lacks ~{~S~^, ~}" file key values))
  (:hostattrs (os:required 'os:freebsd))
  (:apply (sysrc (loop for value in values collect #?"${key}-=${value}")
                 file)))
