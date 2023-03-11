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
;;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

(in-package :consfigurator.property.apache)
(named-readtables:in-readtable :consfigurator)

(defproplist installed :posix ()
  (:desc "Apache installed")
  (os:etypecase
    (debianlike (apt:installed "apache2"))))

(defproplist reloaded :posix ()
  (:desc "Apache reloaded")
  (service:reloaded "apache2"))

(defprop %mod-enabled :posix (name)
  (:hostattrs (os:required 'os:debianlike))
  (:check (zerop (mrun :for-exit "a2query" "-q" "-m" name)))
  (:apply (mrun "a2enmod" "--quiet" name))
  (:unapply (mrun "a2dismod" "--quiet" name)))

(defproplist mod-enabled :posix (name)
  (:desc #?"Apache module ${name} enabled")
  (installed)
  (on-change (%mod-enabled name)
    (reloaded)))

(defproplist conf-available :posix (name config)
  (:desc #?"Apache conf ${name} available")
  (file:exists-with-content
   (merge-pathnames (strcat name ".conf") #P"/etc/apache2/conf-available/")
   config))

(defprop %conf-enabled :posix (name)
  (:hostattrs (os:required 'os:debianlike))
  (:check (zerop (mrun :for-exit "a2query" "-q" "-c" name)))
  (:apply (mrun "a2enconf" "--quiet" name))
  (:unapply (mrun "a2disconf" "--quiet" name)))

(defpropspec conf-enabled :posix (name &optional config)
  (:desc #?"Apache configuration ${name} enabled")
  `(eseqprops
    (installed)
    (on-change ,(if config
                    `(eseqprops (conf-available ,name ,config)
                                (%conf-enabled ,name))
                    `(%conf-enabled ,name))
      (reloaded))))

(defproplist site-available :posix (domain config)
  (:desc #?"Apache site ${domain} available")
  (file:exists-with-content
   (merge-pathnames (strcat domain ".conf") #P"/etc/apache2/sites-available/")
   config))

(defprop %site-enabled :posix (domain)
  (:hostattrs (os:required 'os:debianlike))
  (:check (zerop (mrun :for-exit "a2query" "-q" "-s" domain)))
  (:apply (mrun "a2ensite" "--quiet" domain))
  (:unapply (mrun "a2dissite" "--quiet" domain)))

(defpropspec site-enabled :posix (domain &optional config)
  (:desc #?"Apache site ${domain} enabled")
  `(eseqprops
    (installed)
    (on-change ,(if config
                    `(eseqprops (site-available ,domain ,config)
                                (%site-enabled ,domain))
                    `(%site-enabled ,domain))
      (reloaded))))

(defpropspec https-vhost :posix
    (domain htdocs agree-tos
            &key aliases additional-config additional-config-https)
  "Configure an HTTPS Apache virtual host using a Let's Encrypt certificate.
ALIASES are the values for ServerAlias entries; these must be specified
separately for proper handling of the redirects from HTTP to HTTPS.  Use of
this property implies agreement with the Let's Encrypt Subscriber Agreement;
AGREE-TOS is an instance of LETS-ENCRYPT:AGREE-TOS.  ADDITIONAL-CONFIG are
additional lines to add to the Apache configuration for both the HTTP and
HTTPS virtual hosts; ADDITIONAL-CONFIG-HTTPS are additional lines to be added
only to the HTTPS virtual host.

Unapplying removes the Apache site config but leaves the certificate behind.

The current implementation does not install a certificate renewal hook to
restart Apache."
  `(with-unapply
     (network:aliases ,domain ,@aliases)
     (mod-enabled "ssl")
     (conf-enabled "stapling"
                   ("SSLStaplingCache shmcb:/tmp/stapling_cache(128000)"))
     (mod-enabled "rewrite")
     (site-enabled
      ,domain
      ,(let ((initial `(,(strcat "DocumentRoot " htdocs)
                        "ErrorLog /var/log/apache2/error.log"
                        "LogLevel warn"
                        "CustomLog /var/log/apache2/access.log combined"
                        "ServerSignature on")))
         `(,(strcat "<IfFile " (unix-namestring
                                (lets-encrypt:certificate-for domain))
                    ">")
           "<VirtualHost *:443>"
           ,(strcat "ServerName " domain ":443")
           ,@(loop for alias in aliases collect (strcat "ServerAlias " alias))
           ,@initial
           "SSLEngine on"
           ,(strcat "SSLCertificateFile "
                    (unix-namestring (lets-encrypt:certificate-for domain)))
           ,(strcat "SSLCertificateKeyFile "
                    (unix-namestring (lets-encrypt:privkey-for domain)))
           ,(strcat "SSLCertificateChainFile "
                    (unix-namestring (lets-encrypt:chain-for domain)))
           "SSLUseStapling on"
           ,@additional-config
           ,@additional-config-https
           "</VirtualHost>" "</IfFile>"
           ,@(loop for name in (cons domain aliases) append
                   `(""
                     "<VirtualHost *:80>"
                     ,(strcat "ServerName " name ":80")
                     ,@initial
                     "RewriteEngine On"
                     "RewriteRule ^/.well-known/acme-challenge.* - [L]"
                     ,@additional-config
                     ;; redirect everything else to https
                     "RewriteRule (.*) https://%{SERVER_NAME}$1 [R=301,L,NE]"
                     "</VirtualHost>")))))
     (on-change (lets-encrypt:certificate-obtained
                 ,agree-tos ,htdocs ,domain ,@aliases)
       (reloaded))
     :unapply
     (unapplied (site-enabled ,domain))
     (unapplied (site-available ,domain ""))))
