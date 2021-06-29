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

(in-package :consfigurator.property.network)
(named-readtables:in-readtable :consfigurator)

(defprop aliases :posix (&rest aliases)
  "Record other DNS names by which the host is known.  For example, a mail
server might have aliases like imap.example.org and smtp.example.org, even
though its hostname is neither 'imap' nor 'smtp'."
  (:desc (format nil "Has alias~1{~#[es~;~;es~]~} ~:*~{~A~^, ~}" aliases))
  (:hostattrs (apply #'pushnew-hostattrs
                     :aliases (delete (get-hostname) (flatten aliases)
                                      :test #'string=))))

(defprop ipv4 :posix (&rest addresses)
  "Record the host's public Internet IPv4 addresses.

If you need to record other addresses in hostattrs, such as on a LAN, write a
similar property which pushes hostattrs identified by a non-keyword
symbol (unless your consfig deals only in hosts without public IP addresses,
in which case you can use this property)."
  (:desc (format nil "Has public IPv4 ~{~A~^, ~}" addresses))
  (:hostattrs (apply #'pushnew-hostattrs :ipv4 (flatten addresses))))

(defprop ipv6 :posix (&rest addresses)
  "Record the host's public Internet IPv6 addresses.

If you need to record other addresses in hostattrs, such as on a LAN, write a
similar property which pushes hostattrs identified by a non-keyword
symbol (unless your consfig deals only in hosts without public IP addresses,
in which case you can use this property)."
  (:desc (format nil "Has public IPv6 ~{~A~^, ~}" addresses))
  (:hostattrs (apply #'pushnew-hostattrs :ipv6 (flatten addresses))))

(defprop static :posix (interface address &optional gateway &rest options)
  "Configures an interface with a static IP address.
OPTIONS is a list of even length of alternating keys and values."
  (:desc #?"Static interface ${interface} configured")
  ;; We don't push ADDRESS as an :IPV4 hostattr because perhaps it is not an
  ;; address on the public Internet.
  (:hostattrs (os:required 'os:debianlike))
  (:apply
   (when gateway
     (setq options (list* "gateway" gateway options)))
   (setq options (list* "address" address options))
   (file:has-content
       (merge-pathnames (string->filename interface)
                        #P"/etc/network/interfaces.d/")
     (list* (strcat "auto " interface)
            (format nil "iface ~A ~A static"
                    interface (if (find #\. address) "inet" "inet6"))
            (loop for (k v) on options by #'cddr
                  collect (format nil "	~A ~A" k v))))))
