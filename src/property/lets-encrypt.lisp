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

(in-package :consfigurator.property.lets-encrypt)
(named-readtables:in-readtable :consfigurator)

(defproplist installed :posix ()
  (:desc "Let's Encrypt client installed")
  (os:etypecase
    (debianlike (apt:installed "certbot"))))

(defclass agree-tos ()
  ((email-address :initarg :email-address))
  (:documentation
   "Object representing your agreement with the Let's Encrypt Subscriber
Agreement; you will need to pass this to properties which will invoke the
Let's Encrypt client.  Supply an e-mail address so that Let's Encrypt can
contact you for things like certificate expiry, planned outage notifications
etc."))

(define-print-object-for-structlike agree-tos)

(defmacro agree-tos (&key (email-address nil email-address-supplied-p))
  `(make-instance 'agree-tos ,@(and email-address-supplied-p
                                    `(:email-address ,email-address))))

;; Based on Propellor's LetsEncrypt.letsEncrypt' property.
(defprop %obtained :posix (agree-tos domains &rest args)
  (:apply
   (check-type agree-tos agree-tos)
   (let ((dir (ensure-directory-pathname
               (merge-pathnames (car domains) #P"/etc/letsencrypt/live/"))))
     (with-change-if-changes-files ((merge-pathnames "cert.pem" dir)
                                    (merge-pathnames "chain.pem" dir)
                                    (merge-pathnames "privkey.pem" dir)
                                    (merge-pathnames "fullchain.pem" dir))
       (mrun "letsencrypt" "certonly" "--agree-tos"
             (if (slot-boundp agree-tos 'email-address)
                 (strcat "--email=" (slot-value agree-tos 'email-address))
                 "--register-unsafely-without-email")
             args "--text" "--noninteractive" "--keep-until-expiring"
             ;; Always request expansion in case DOMAINS has changed.
             "--expand"
             (loop for domain in domains
                   when (and (stringp domain) (plusp (length domain)))
                     collect (strcat "--domain=" domain)))))))

(defproplist certificate-obtained :posix (agree-tos htdocs &rest domains)
  "Obtains, and renews as necessary, an SSL certificate for DOMAINS.
The first element of DOMAINS, after flattening, is the Common Name of the
certificate.  Use of this property implies agreement with the Let's Encrypt
Subscriber Agreement; AGREE-TOS is an instance of LETS-ENCRYPT:AGREE-TOS.
HTDOCS is the web root for DOMAINS, which must be writeable, and publically
available over plain HTTP.

This property does nothing to ensure that your web server will actually use
the obtained certificate.  Typically you'll want to combine this property with
web server-specific properties in a DEFPROPLIST/DEFPROPSPEC."
  (:desc (format nil "Let's Encrypt for ~{~A~^, ~}" domains))
  (installed)
  (%obtained agree-tos (flatten domains) "--webroot" "--webroot-path" htdocs))

(defproplist certificate-obtained-standalone :posix (agree-tos &rest domains)
  "Like LETS-ENCRYPT:CERTIFICATE-OBTAINED, but use the --standalone argument to
letsencrypt(1) to start up the client's built-in webserver on port 80.  Useful
on hosts which do not normally run a web server, but nevertheless require an
SSL certificate for other service(s), such as mail servers."
  (:desc (format nil "Let's Encrypt for ~{~A~^, ~}" domains))
  (installed)
  (%obtained agree-tos (flatten domains)
             "--standalone" "--preferred-challenges" "http"))

(defun dir-for (domain)
  (ensure-directory-pathname
   (merge-pathnames domain #P"/etc/letsencrypt/live/")))

(defun fullchain-for (domain)
  (merge-pathnames "fullchain.pem" (dir-for domain)))

(defun chain-for (domain)
  (merge-pathnames "chain.pem" (dir-for domain)))

(defun certificate-for (domain)
  (merge-pathnames "cert.pem" (dir-for domain)))

(defun privkey-for (domain)
  (merge-pathnames "privkey.pem" (dir-for domain)))
