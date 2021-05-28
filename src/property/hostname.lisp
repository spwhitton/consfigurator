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

(in-package :consfigurator.property.hostname)
(named-readtables:in-readtable :consfigurator)

(defun domain (hostname)
  (subseq hostname (min (length hostname) (1+ (position #\. hostname)))))

(defprop is :posix (hostname)
  "Specify that the hostname of this host is HOSTNAME.
Useful for hosts implicitly defined inline using dotted propapp notation.
Unlikely to be useful for hosts defined using DEFHOST."
  (:hostattrs (push-hostattrs :hostname hostname)))

(defpropspec configured :posix
    (&optional (hostname (get-hostname) hostname-supplied-p)
               (domain (domain hostname))
               &aux (short (car (split-string hostname :separator "."))))
  "Set the hostname in the standard Debian way.
When HOSTNAME is an FQDN, DOMAIN is the domain part of the hostname,
defaulting to everything after the first dot.  (For some hosts, the domain
should rather be the whole hostname.)"
  (:desc "Hostname configured")
  `(seqprops
    ,@(and hostname-supplied-p `((is ,hostname)))
    (on-change (file:has-content "/etc/hostname" ,short)
      (container:when-contained (:hostname)
        (cmd:single ,(strcat "hostname " short))))
    (file:contains-conf-tab
     "/etc/hosts"
     ,@(and (plusp (length domain))
            `("127.0.1.1" ,(strcat hostname " " short)))
     "127.0.0.1" "localhost")))

(defproplist mailname-configured :posix (&optional (mailname (get-hostname)))
  "Sets the mailname to MAILNAME.
The FQDN is a good choice for unclustered machines which do not have
publically advertised MX records; in other cases it will often be better to
use only the domain name portion of the hostname."
  (:desc "/etc/mailname configured")
  (file:has-content "/etc/mailname" mailname))

(defproplist search-configured :posix (&optional (domain (domain (get-hostname))))
  "Set the default DOMAIN for hostname lookup in /etc/resolv.conf.
DOMAIN defaults to everything after the first dot of the machine's hostname,
which is assumed to be an FQDN."
  (:desc "Search domain in /etc/resolv.conf configured")
  (file:contains-conf-space "/etc/resolv.conf" "search" domain))
