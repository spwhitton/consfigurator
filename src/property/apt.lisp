;;; Consfigurator -- Lisp declarative configuration management system

;;; Copyright (C) 2017, 2021  Sean Whitton <spwhitton@spwhitton.name>

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

(in-package :consfigurator.property.apt)
(named-readtables:in-readtable :consfigurator)


;;;; Static definitions

(defmacro with-maybe-update (form)
  `(handler-case ,form
     (run-failed ()
       (updated)
       ,form)))

(defmacro with-changes-dpkg-status (&body forms)
  `(with-change-if-changes-file-content ("/var/lib/dpkg/status") ,@forms))

(define-constant +sections+ '("main" "contrib" "non-free") :test #'equal)

(define-constant +noninteractive-env+ '(:DEBIAN_FRONTEND "noninteractive"
                                        :APT_LISTCHANGES_FRONTEND "none")
  :test #'equal)


;;;; Properties

(defprop installed :posix (&rest packages)
  "Ensure all of the apt packages PACKAGES are installed."
  (:desc #?"apt installed @{packages}")
  (:preprocess (flatten packages))
  (:hostattrs (os:required 'os:debianlike))
  (:check (all-installed-p packages))
  (:apply
   (with-maybe-update (apt-get :inform "-y" "install" packages))))

(defprop installed-minimally :posix (&rest packages)
  "Ensure all of the apt packages PACKAGES are installed, without recommends."
  (:desc #?"apt installed @{packages}")
  (:preprocess (flatten packages))
  (:hostattrs (os:required 'os:debianlike))
  (:check (all-installed-p packages))
  (:apply
   (with-maybe-update
       (apt-get :inform "-y" "--no-install-recommends" "install" packages))))

(defprop removed :posix (&rest packages)
  "Ensure all of the apt packages PACKAGES are removed."
  (:desc #?"apt removed @{packages}")
  (:preprocess (flatten packages))
  (:hostattrs
   (declare (ignore packages))
   (os:required 'os:debianlike))
  (:check
   (none-installed-p packages))
  (:apply
   (apt-get :inform "-y" "remove" packages)))

(defprop reconfigured :posix (package &rest triples)
  "Where each of TRIPLES is a list of three strings, a debconf template, type
and value, debconf reconfigure PACKAGE with those new debconf values.
Typically used with the ON-CHANGE combinator."
  (:desc (declare (ignore triples)) #?"${package} reconfigured")
  (:hostattrs
   (declare (ignore package triples))
   (os:required 'os:debianlike))
  (:apply
   (assert-euid-root)
   (run
    :input (unlines
            (loop for triple in triples collect #?"${package} @{triple}"))
    "debconf-set-selections")
   (run :env +noninteractive-env+ "dpkg-reconfigure" "-fnone" package)))

(defproplist service-installed-running :posix (package)
  "Where PACKAGE installs a service named PACKAGE, ensure it is installed and
running.

E.g. (APT:SERVICE-INSTALLED-RUNNING \"apache2\")."
  (:desc #?"${package} installed and running")
  (installed package)
  (service:running package))

(defprop all-configured :posix ()
  "Ensure that there are no packages for which installation is unfinished."
  (:desc "dpkg --configure --pending")
  (:hostattrs (os:required 'os:debianlike))
  (:apply
   (with-changes-dpkg-status
     (run :env +noninteractive-env+ "dpkg" "--configure" "--pending"))))

(defproplist updated :posix ()
  "Ensure the apt indexes are up-to-date."
  (:desc "apt-get update")
  (all-configured)
  (os:etypecase
    (debian-stable
     (cmd:single :env +noninteractive-env+ :inform "apt-get" "update"))
    (debian
     (cmd:single :env +noninteractive-env+ :inform
                 "apt-get" "update" "--allow-releaseinfo-change"))))

(defprop upgraded :posix ()
  (:desc "apt upgraded")
  (:hostattrs (os:required 'os:debianlike))
  (:apply (with-changes-dpkg-status
            (apt-get :inform "-y" "dist-upgrade"))))

(defprop autoremoved :posix ()
  (:desc "apt removed automatically installed packages")
  (:hostattrs (os:required 'os:debianlike))
  (:apply (with-changes-dpkg-status
            (apt-get :inform "-y" "autoremove"))))

(defprop periodic-updates :posix ()
  "Enable periodically updating the apt indexes and downloading new versions of
packages.  Does not do any automatic upgrades."
  (:desc "apt periodic updates")
  (:hostattrs (os:required 'os:debianlike))
  (:apply
   (file:has-content "/etc/apt/apt.conf.d/02periodic"
#>EOF>APT::Periodic::Enable "1";
APT::Periodic::Update-Package-Lists "1";
APT::Periodic::Download-Upgradeable-Packages "1";
APT::Periodic::Verbose "1";
EOF))
  (:unapply
   (file:does-not-exist "/etc/apt/apt.conf.d/02periodic")))

(defproplist unattended-upgrades :posix ()
  "Enable unattended upgrades.

Note that in its default configuration on Debian, unattended-upgrades will
only upgrade Debian stable."
  (:desc "Unattended upgrades enabled")
  (with-unapply
   (on-change (installed "unattended-upgrades")
     (reconfigured
      "unattended-upgrades"
      '("unattended-upgrades/enable_auto_updates" "boolean" "true")))
   (service:running "cron")
   (desc "unattended-upgrades will mail root"
         (file:contains-lines "/etc/apt/apt.conf.d/50unattended-upgrades"
                              "Unattended-Upgrade::Mail \"root\";"))
   ;; work around Debian bug #812380
   (file:does-not-exist "/etc/apt/apt.conf.d/50unattended-upgrades.ucf-dist")
   :unapply (removed "unattended-upgrades")))

(defprop mirror :posix (uri)
  (:desc #?"${uri} apt mirror selected")
  (:hostattrs
   (pushnew-hostattrs :apt.mirror uri)))

(defpropspec uses-parent-mirrors :posix ()
  (:desc #?"Uses parent's apt mirror(s), if any")
  (let ((mirrors (get-parent-hostattrs :apt-mirror)))
    (and mirrors
         `(eseqprops
           ,@(loop for mirror in mirrors collect `(mirror ,mirror))))))

(defprop proxy :posix (uri)
  (:desc #?"${uri} apt proxy selected")
  (:hostattrs
   (pushnew-hostattrs :apt.proxy uri))
  (:apply
   (file:has-content "/etc/apt/apt.conf.d/20proxy"
                     (format nil "Acquire::HTTP::Proxy \"~A\";~%" uri))))

(defproplist uses-parent-proxy :posix ()
  (:desc #?"Uses parent's apt proxy")
  (proxy (or (get-parent-hostattrs-car :apt.proxy)
             (failed-change "Parent has no apt proxy"))))

(defproplist uses-local-cacher :posix ()
  (:desc "apt uses local apt cacher")
  (service-installed-running "apt-cacher-ng")
  (proxy "http://localhost:3142"))

(defun get-mirrors ()
  (or (get-hostattrs :apt.mirror) (call-with-os #'get-default-mirrors)))

(defmethod get-default-mirrors ((os os:debian))
  '("http://deb.debian.org/debian"))

(defproplist standard-sources.list :posix ()
  (:desc "Standard sources.list")
  (file:has-content "/etc/apt/sources.list"
    (call-with-os #'standard-sources-for)))

(defmethod standard-sources-for ((os os:debian))
  (let* ((suite (os:debian-suite os))
         (archive (mapcar (lambda (m) (list* m suite +sections+))
                          (get-mirrors)))
         (updates (and (subtypep (type-of os) 'os:debian-stable)
                       (mapcar (lambda (m)
                                 (list* m #?"${suite}-updates" +sections+))
                               (get-mirrors))))
         (backports (and (subtypep (type-of os) 'os:debian-stable)
                         (mapcar (lambda (m)
                                   (list* m #?"${suite}-backports" +sections+))
                                 (get-mirrors))))
         (security-suite (if (memstring= suite '("stretch" "jessie" "buster"))
                             #?"${suite}/updates"
                             #?"${suite}-security"))
         (security (and (or (subtypep (type-of os) 'os:debian-stable)
                            (subtypep (type-of os) 'os:debian-testing))
                        (list
                         (list* "http://security.debian.org/debian-security"
                                security-suite +sections+)))))
    (mapcan (lambda (l) (list #?"deb @{l}" #?"deb-src @{l}"))
            (nconc archive updates backports security))))

(defproplist additional-sources :posix (basename content)
  "Add additional apt source lines to a file in /etc/apt/sources.list.d named
after BASENAME.  CONTENT is as the content argument to FILE:HAS-CONTENT."
  (declare (indent 1))
  (on-change (file:exists-with-content
              #?"/etc/apt/sources.list.d/${basename}.list" content)
    (updated)))

(defprop cache-cleaned :posix ()
  "Empty apt's cache to recover disk space."
  (:desc "apt cache cleaned")
  (:hostattrs (os:required 'os:debianlike))
  (:apply (apt-get "clean") :no-change))

(defproplist trusts-key :posix
    (fingerprint &optional (basename (remove #\Space fingerprint))
                 &aux (file #?"/etc/apt/trusted.gpg.d/${basename}.asc"))
  "Have apt trust the PGP key identified by FINGERPRINT to sign apt archives."
  (:desc #?"apt trusts PGP public key ${fingerprint}")
  (with-unapply
    (file:data-uploaded "--pgp-pubkey" (remove #\Space fingerprint) file)
    :unapply (file:does-not-exist file)))

(defproplist no-pdiffs :posix ()
  "Disable the use of PDiffs for machines with high bandwidth connections."
  (file:exists-with-content "/etc/apt/apt.conf.d/20pdiffs"
                            '("Acquire::PDiffs \"false\";")))


;;;; Pinning

(defmethod suite-pin ((os os:debian-stable))
  (strcat "n=" (os:debian-suite os)))

(defmethod suite-pin ((os os:debian))
  (strcat "a=" (os:debian-suite os)))

(defmethod suite-pin-block ((pref string) (os os:debian) pin-priority)
  `("Explanation: This file added by Consfigurator"
    ,(strcat "Package: " pref)
    ,(strcat "Pin: release " (suite-pin os))
    ,(format nil "Pin-Priority: ~D" pin-priority)))

(defpropspec suites-available-pinned :posix (&rest pairs)
  "Where PAIRS is a list of even length of alternating instances of OS:DEBIAN
and apt pin priorities, add an apt source for the instance of OS:DEBIAN and
pin that suite to a given pin value (see apt_preferences(5)).  Unapply to drop
the source and unpin the suite.

If the OS:DEBIAN is the host's OS, the suite is pinned, but no source is
added.  That apt source should already be available, or you can use a property
like APT:STANDARD-SOURCES.LIST."
  (:desc (loop for (os pin) on pairs by #'cddr
               for suite = (os:debian-suite os)
               collect #?{Debian "${suite}" pinned, priority ${pin}}
                 into accum
               finally (return (format nil "~{~A~^; ~}" accum))))
  (:hostattrs (os:required 'os:debian))
  `(eseqprops
    ,@(loop for (os pin) on pairs by #'cddr
            for suite = (os:debian-suite os)
            do (check-type pin integer)
            collect `(file:exists-with-content
                      ,#?"/etc/apt/preferences.d/20${suite}.pref"
                      ,(suite-pin-block "*" os pin))
            unless (and
                    (subtypep (type-of (get-hostattrs-car :os)) 'os:debian)
                    (string= suite (os:debian-suite (get-hostattrs-car :os))))
              ;; Unless we are pinning a backports suite, filter out any
              ;; backports sources that were added by STANDARD-SOURCES-FOR.
              ;; Probably don't want those to be pinned to the same value.
              collect `(additional-sources
                        ,suite ,(if (string-suffix-p suite "-backports")
                                    (standard-sources-for os)
                                    (loop for line in (standard-sources-for os)
                                          unless (search "-backports" line)
                                            collect line))))))

(defpropspec pinned :posix (preferences &rest pairs)
  "Pins a list of packages, package wildcards and/or regular expressions,
PREFERENCES, to a list of suites and corresponding pin priorities.  Unapply to
unpin.  PAIRS is a list of even length of alternating instances of OS:DEBIAN
and apt pin priorities.

Each package, package wildcard or regular expression will be pinned to all of
the specified suites.  Each of PREFERENCES is the name of a package, a glob to
match the names of packages, or a regexp surrounded by slashes to match the
names of packages.  See apt_preferences(5), \"Regular expressions and glob(7)
syntax\".

Note that this will have no effect unless there is an apt source for each of
the suites.  One way to add an apt source is APT:SUITES-AVAILABLE-PINNED.

For example, to obtain Emacs Lisp addon packages not present in your stable
release of Debian from testing, falling back to sid if they're not available
in testing, you could use:

    (os:debian-stable \"bullseye\" :amd64)
    (apt:suites-available-pinned (os:debian-testing)  -10
                                 (os:debian-unstable) -10)
    (apt:pinned '(\"elpa-*\")
                (os:debian-testing)  100
                (os:debian-unstable) 50)"
  (:desc (loop for (os pin) on pairs by #'cddr
               for suite = (os:debian-suite os)
               collect #?{Debian "${suite}", priority ${pin}} into accum
               finally (return (format nil "~{~A~^, ~} pinned to ~{~A~^; ~}"
                                       preferences accum))))
  (:hostattrs (os:required 'os:debian))
  `(eseqprops
    ,@(loop for preference in preferences
            collect (list
                     'file:exists-with-content
                     (strcat "/etc/apt/preferences.d/10consfig_"
                             (string->filename preference)
                             ".pref")
                     (nbutlast
                      (loop for (os pin) on pairs by #'cddr
                            nconc (suite-pin-block preference os pin)
                            collect ""))))))


;;;; Reports on installation status

(defun apt-cache-policy (packages)
  (runlines :env '(:LANG "C") "apt-cache" "policy" packages))

(define-constant apt-cache-policy-installed #?/^\s+Installed:\s+(?!\(none\))/
  :test #'string=)

(defun all-installed-p (&rest packages)
  (loop with n = 0
        with packages* = (flatten packages)
        for line in (apt-cache-policy packages*)
        when (re:scan apt-cache-policy-installed line)
          do (incf n)
        finally (return (= n (length packages*)))))

(defun none-installed-p (&rest packages)
  (loop for line in (apt-cache-policy (flatten packages))
        never (re:scan apt-cache-policy-installed line)))


;;;; Utilities

(defun apt-get (&rest args)
  (apply #'run :env +noninteractive-env+ "apt-get" args))
