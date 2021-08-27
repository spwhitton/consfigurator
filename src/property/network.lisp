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
  (:hostattrs
   (pushnew-hostattrs
    :aliases (delete (get-hostname) (flatten aliases) :test #'string=))))

(defprop ipv4 :posix (&rest addresses)
  "Record the host's public Internet IPv4 addresses.

If you need to record other addresses in hostattrs, such as on a LAN, write a
similar property which pushes hostattrs identified by a non-keyword
symbol (unless your consfig deals only in hosts without public IP addresses,
in which case you can use this property)."
  (:desc (format nil "Has public IPv4 ~{~A~^, ~}" addresses))
  (:hostattrs (pushnew-hostattrs :ipv4 (flatten addresses))))

(defprop ipv6 :posix (&rest addresses)
  "Record the host's public Internet IPv6 addresses.

If you need to record other addresses in hostattrs, such as on a LAN, write a
similar property which pushes hostattrs identified by a non-keyword
symbol (unless your consfig deals only in hosts without public IP addresses,
in which case you can use this property)."
  (:desc (format nil "Has public IPv6 ~{~A~^, ~}" addresses))
  (:hostattrs (pushnew-hostattrs :ipv6 (flatten addresses))))

(defproplist clean-/etc/network/interfaces :posix ()
  "Empty /etc/network/interfaces in preparation for configuring interfaces using
/etc/network/interfaces.d.  On fresh installs this property should not be
necessary, but it is useful for removing configuration inserted by your VPS
hosting provider, for example."
  (:hostattrs (os:required 'os:debianlike))
  (file:has-content "/etc/network/interfaces"
    ;; This is the contents of the file on fresh Debian "bullseye" installs --
    ;; the IPv4 loopback interface is no longer configured here.
    '("# interfaces(5) file used by ifup(8) and ifdown(8)"
      "# Include files from /etc/network/interfaces.d:"
      "source /etc/network/interfaces.d/*")))

(defprop static :posix
    (interface address &optional gateway netmask &rest options)
  "Configures an interface with a static IP address.
OPTIONS is a list of even length of alternating keys and values."
  (:desc #?"Static interface ${interface} configured")
  ;; We don't push ADDRESS as an :IPV4 hostattr because perhaps it is not an
  ;; address on the public Internet.
  (:hostattrs (os:required 'os:debianlike))
  (:apply
   (when gateway
     (setq options (list* "gateway" gateway options)))
   (when netmask
     (setq options (list* "netmask" netmask options)))
   (setq options (list* "address" address options))
   (file:has-content
       (merge-pathnames (string->filename interface)
                        #P"/etc/network/interfaces.d/")
     (list* (strcat "auto " interface)
            (format nil "iface ~A ~A static"
                    interface (if (find #\. address) "inet" "inet6"))
            (loop for (k v) on options by #'cddr
                  collect (format nil "	~A ~A" k v))))))

;; Based on Propellor's Network.preserveStatic property.
(defprop preserve-static-once :posix (&optional interface &rest options)
  "Writes configuration to bring up INTERFACE, statically, with the IP addresses
and routing configuration currently associated with the interface, assuming
that INTERFACE has already been brought up by other means, such as DHCP.
INTERFACE defaults to the interface of the default route.  This property does
nothing if the interface configuration file already exists.  OPTIONS is a list
of even length of alternating keys and values.

IPv6 addresses are ignored, as it is assumed these use stateless configuration
of some form, which is best implemented using a property which does not query
the networking stack's current state like this one does."
  (:hostattrs (os:required 'os:debianlike))
  (:apply
   (let* ((default
            (loop for line in (runlines "ip" "route" "list" "scope" "global")
                  when (string-prefix-p "default " line)
                    return (words line)))
          (interface (or interface (fifth default)))
          (gateway (and (string= (fifth default) interface) (third default)))
          (file (merge-pathnames (string->filename interface)
                                 #P"/etc/network/interfaces.d/")))
     (if (remote-exists-p file)
         :no-change
         (file:has-content file
           (cons
            (strcat "auto " interface)
            (loop for line in (runlines "ip" "-o" "addr" "show" interface
                                        "scope" "global")
                  for fields = (words line)
                  when (string= "inet" (third fields))
                    collect (strcat "iface " interface " inet static")
                    and nconc (multiple-value-bind (addr nm)
                                  (parse-cidr (fourth fields))
                                (list (strcat "	address " addr)
                                      (strcat "	netmask " nm)))
                    and if gateway collect (strcat "	gateway " gateway)
                          end
                    and nconc
                        (loop for (k v) on options by #'cddr
                              collect (format nil "	~A ~A" k v)))))))))
