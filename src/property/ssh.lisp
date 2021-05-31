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

(in-package :consfigurator.property.ssh)
(named-readtables:in-readtable :consfigurator)

(defprop authorized-keys :posix (&rest keys)
  "Permits using KEYS to SSH in as the current user."
  (:desc (declare (ignore keys))
         (strcat (get-connattr :remote-user) " has authorized_keys"))
  (:apply
   (file:directory-exists ".ssh")
   (apply #'file:contains-lines ".ssh/authorized_keys" keys))
  (:unapply
   (apply #'file:lacks-lines ".ssh/authorized_keys" keys)))

(defprop %update-known-hosts :posix (file host &key short-hostname)
  (:apply
   (file:map-file-lines
    file
    (lambda (lines)
      (loop with (identifier . keys)
              = (sshd:get-host-public-keys host :short-hostname short-hostname)
            for line in lines
            for index = (position #\Space line)
            for line-identifier = (subseq line 0 index)
            and line-key = (subseq line (1+ index))
            when (or (not (string= line-identifier identifier))
                     (member line-key keys :test #'string=))
              collect line into accum
              and do (deletef keys line-key :test #'string=)
            finally
               (return
                 (nconc accum
                        (loop for key in keys
                              collect (format nil "~A ~A" identifier key))))))))
  (:unapply
   (destructuring-bind (identifier . keys)
       (sshd:get-host-public-keys host :short-hostname short-hostname)
     (file:lacks-lines file
                       (loop for key in keys
                             collect (format nil "~A ~A" identifier key))))))

(defproplist known-host :posix (host &key short-hostname)
  "Ensures that the SSH host keys of HOST are stored in ~/.ssh/known_hosts.
If SHORT-HOSTNAME, include the part of HOST's hostname before the first dot as
one of the hostnames identifying HOST.  Removes any other host keys
identifying HOST, to simplify refreshing keys."
  (:desc #?"${(get-hostname host)} is known host to ssh client")
  (file:directory-exists ".ssh")
  (%update-known-hosts ".ssh/known_hosts" host :short-hostname short-hostname))

(defproplist globally-known-host :posix (host &key short-hostname)
  "Ensures that SSH host keys of HOST are stored in /etc/ssh/ssh_known_hosts.
If SHORT-HOSTNAME, include the part of HOST's hostname before the first dot as
one of the hostnames identifying HOST.  Removes any other host keys
identifying HOST, to simplify refreshing keys."
  (:desc #?"${(get-hostname host)} is globally known host to ssh client")
  (%update-known-hosts
   "/etc/ssh/ssh_known_hosts" host :short-hostname short-hostname))

(defproplist parent-is-globally-known-host :posix (&key short-hostname)
  "Ensures that the SSH host keys of the parent host are stored in
/etc/ssh/ssh_known_hosts; SHORT-HOSTNAME is as for SSH:GLOBALLY-KNOWN-HOST."
  (:desc "Parent host is globally known host to ssh client")
  (%update-known-hosts
   "/etc/ssh/ssh_known_hosts" (make-host :hostattrs
                                         (get-hostattrs :parent-hostattrs))
   :short-hostname short-hostname))
