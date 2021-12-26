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
   (apply #'file:contains-lines ".ssh/authorized_keys" keys))
  (:unapply
   (apply #'file:lacks-lines ".ssh/authorized_keys" keys)))

(defpropspec has-user-key :posix (dest public-key &key iden1)
  "Installs an SSH keypair to DEST and DEST.pub."
  ;; The original version of this property took a key type argument and
  ;; defaulted DEST to ~/.ssh/id_TYPE, but FILE:HOST-SECRET-UPLOADED requires
  ;; an absolute path because the remote HOME is not known at :HOSTATTRS time,
  ;; and the same applies here, so the caller must supply DEST.  In the
  ;; FILE:SECRET-UPLOADED branch we could use a relative path, but we should
  ;; not use an identical relative path for both IDEN2 and the destination
  ;; when IDEN1 is a hostname, which it might be.
  `(eseqprops (file:exists-with-content
               ,(strcat (unix-namestring dest) ".pub") ,public-key)
              ,(if iden1
                   `(file:secret-uploaded ,iden1 ,dest ,dest)
                   `(file:host-secret-uploaded ,dest))))

(defprop %update-known-hosts :posix
    (file host &key short-hostname (aliases t) (ips t) additional-names)
  (:apply
   (file:map-file-lines
    file
    (lambda (lines)
      (loop with host = (preprocess-host host)
            with (identifier . keys)
              = (sshd:get-host-public-keys
                 host :aliases aliases :short-hostname short-hostname
                 :ips ips :additional-names additional-names)
            and hostname = (get-hostname host)
            for line in lines
            for comma = (position #\, line) and space = (position #\Space line)
            for index = (if comma (min comma space) space)
            for line-hostname = (subseq line 0 index)
            and line-key = (subseq line (1+ space))
            unless (string= line-hostname hostname)
              collect line into accum
            else if (member line-key keys :test #'string=)
                   collect (format nil "~A ~A" identifier line-key) into accum
                   and do (deletef keys line-key :test #'string=)
            finally
               (return
                 (nconc accum
                        (loop for key in keys
                              collect (format nil "~A ~A" identifier key))))))))
  (:unapply
   (destructuring-bind (identifier . keys)
       (sshd:get-host-public-keys
        host :aliases aliases :short-hostname short-hostname
        :ips ips :additional-names additional-names)
     (file:lacks-lines file
                       (loop for key in keys
                             collect (format nil "~A ~A" identifier key))))))

(defproplist known-host :posix (host &key short-hostname (aliases t)
                                     (ips t) additional-names)
  "Ensures that the SSH host keys of HOST are stored in ~/.ssh/known_hosts.
If SHORT-HOSTNAME, include the part of HOST's hostname before the first dot as
one of the hostnames identifying HOST.  Removes any other host keys
identifying HOST, to simplify refreshing keys."
  (:desc #?"${(get-hostname host)} is known host to ssh client")
  (file:directory-exists ".ssh")
  (%update-known-hosts ".ssh/known_hosts" host
                       :aliases aliases :short-hostname short-hostname
                       :ips ips :additional-names additional-names))

(defproplist globally-known-host :posix (host &key short-hostname (aliases t)
                                              (ips t) additional-names)
  "Ensures that SSH host keys of HOST are stored in /etc/ssh/ssh_known_hosts.
If SHORT-HOSTNAME, include the part of HOST's hostname before the first dot as
one of the hostnames identifying HOST.  Removes any other host keys
identifying HOST, to simplify refreshing keys."
  (:desc #?"${(get-hostname host)} is globally known host to ssh client")
  (%update-known-hosts
   "/etc/ssh/ssh_known_hosts" host
   :aliases aliases :short-hostname short-hostname
   :ips ips :additional-names additional-names))

(defproplist parent-is-globally-known-host :posix
    (&key short-hostname (aliases t) (ips t) additional-names)
  "Ensures that the SSH host keys of the parent host are stored in
/etc/ssh/ssh_known_hosts; SHORT-HOSTNAME is as for SSH:GLOBALLY-KNOWN-HOST."
  (:desc "Parent host is globally known host to ssh client")
  (%update-known-hosts
   "/etc/ssh/ssh_known_hosts" (make-host :hostattrs
                                         (get-hostattrs :parent-hostattrs))
   :short-hostname short-hostname
   :aliases aliases :ips ips :additional-names additional-names))
