;;; Consfigurator -- Lisp declarative configuration management system

;;; Copyright (C) 2017, 2021-2022, 2025  Sean Whitton <spwhitton@spwhitton.name>

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

(define-constant +sections+ '("main" "contrib" "non-free-firmware" "non-free")
  :test #'equal)

(define-constant +noninteractive-env+ '(:DEBIAN_FRONTEND "noninteractive"
                                        :APT_LISTCHANGES_FRONTEND "none")
  :test #'equal)

(defconstant +dpkg-lock-timeout+ 60)


;;;; Properties

;; Cache what we've installed and removed this deployment, rather than
;; checking over and over again.  We assume, then, that no other properties
;; add or remove packages in a way that could invalidate assumptions made by
;; implementations of properties subsequently to be applied.
;; To be safe, user properties that install or remove packages by calling
;; apt-get(8) or dpkg(1) directly should invalidate the cache by calling
;; APT:KNOWN-INSTALLED-REMOVED-PACKAGES-RESET.
(defun install-remove
    (args packages check-against add-to remove-from &aux all)
  "Unless each of PACKAGES appears in the union of the connattrs named by the
elements of CHECK-AGAINST, execute apt-get(8) on ARGS.
Then add each of PACKAGES to each of the connattrs named by the elements of
ADD-TO and remove each of PACKAGES from each of the connattrs named by the
elements of REMOVE-FROM."
  (if (subsetp packages (reduce (lambda (x y)
                                  (union x (get-connattr y) :test #'string=))
                                (ensure-list check-against)
                                :initial-value nil)
               :test #'string=)
      :no-change
      (prog1 (with-maybe-update
                 (with-changes-dpkg-status (apt-get :inform args)))
        ;; We cache just what we've explicitly installed or removed, but other
        ;; packages may have been installed or removed too.
        (dolist (connattr (ensure-list add-to))
          (unionf (get-connattr connattr) packages :test #'string=)
          (push connattr all))
        (dolist (connattr (ensure-list remove-from))
          (setf (get-connattr connattr)
                (nset-difference (get-connattr connattr) packages
                                 :test #'string=))
          (push connattr all))
        (apply #'informat 3
               "~&~@{~@[Known ~(~A~) packages now: ~{~A~^, ~}~%~]~}"
               (loop for connattr in all
                     collect connattr
                     collect (get-connattr connattr))))))

(defprop known-installed-removed-packages-reset :posix ()
  "Reset the lists of known-installed and known-removed Debian packages.
You should call this in custom properties that manually manipulate what
packages are installed in order to ensure that subsequent applications of APT
properties do not assume that their work has already been done."
  (:desc "Lists of installed and removed packages reset")
  (:apply (setf (get-connattr 'installed) nil
                (get-connattr 'removed) nil
                (get-connattr 'installed-backports) nil)))

(defprop installed :posix (&rest packages)
  "Ensure all of the apt packages PACKAGES are installed."
  (:desc #?"apt installed @{packages}")
  (:preprocess (flatten packages))
  (:hostattrs (os:required 'os:debianlike))
  (:apply
   (install-remove (list* "-y" "install" packages) packages
                   '(installed installed-backports) 'installed 'removed)))

(defprop installed-minimally :posix (&rest packages)
  "Ensure all of the apt packages PACKAGES are installed, without recommends."
  (:desc #?"apt installed @{packages}")
  (:preprocess (flatten packages))
  (:hostattrs (os:required 'os:debianlike))
  (:apply
   (install-remove (list* "-y" "--no-install-recommends" "install" packages)
                   packages
                   '(installed installed-backports) 'installed 'removed)))

(defun install-backports (args packages)
  (install-remove
   (append args '("install")
           (loop with suite = (os:debian-suite (get-hostattrs-car :os))
                 for pkg in packages
                 collect (format nil "~A/~A-backports" pkg suite)))
   packages
   'installed-backports 'installed-backports '(installed removed)))

(defprop backports-installed :posix (&rest packages)
  "Ensure all of the apt packages PACKAGES are installed from stable-backports.

Note that if installing any of the backports requires installing versions of
the backport's dependencies from stable-backports too, you will need to list
each of those dependencies in PACKAGES."
  (:desc (format nil "apt installed backport~P ~{~A~^ ~}"
                 (length packages) packages))
  (:preprocess (flatten packages))
  (:hostattrs (os:required 'os:debian-stable))
  (:apply (install-backports '("-y") packages)))

(defprop backports-installed-minimally :posix (&rest packages)
  "Like APT:BACKPORTS-INSTALLED but don't install recommends."
  (:desc (format nil "apt installed backport~P ~{~A~^ ~}"
                 (length packages) packages))
  (:preprocess (flatten packages))
  (:hostattrs (os:required 'os:debian-stable))
  (:apply (install-backports '("-y" "--no-install-recommends") packages)))

(defprop removed :posix (&rest packages)
  "Ensure all of the apt packages PACKAGES are removed."
  (:desc #?"apt removed @{packages}")
  (:preprocess (flatten packages))
  (:hostattrs
   (declare (ignore packages))
   (os:required 'os:debianlike))
  (:apply
   (install-remove (list* "-y" "remove" packages) packages
                   'removed 'removed '(installed installed-backports))))

(defprop reconfigured :posix (package &rest triples)
  "Where each of TRIPLES is a list of three strings, a debconf template, type
and value, debconf reconfigure PACKAGE with those new debconf values.
Typically used with the ON-CHANGE combinator."
  (:desc (declare (ignore triples)) #?"${package} reconfigured")
  (:hostattrs
   (declare (ignore package triples))
   (os:required 'os:debianlike))
  (:apply
   (assert-remote-euid-root)
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
  (seqprops
   (all-configured)
   (os:etypecase
     (debian-stable
      (cmd:single :env +noninteractive-env+ :inform "apt-get"
                  "-o" #?"DPkg::Lock::Timeout=${+dpkg-lock-timeout+}"
                  "update"))
     (debian
      (cmd:single :env +noninteractive-env+ :inform
                  "apt-get"
                  "-o" #?"DPkg::Lock::Timeout=${+dpkg-lock-timeout+}"
                  "update" "--allow-releaseinfo-change")))))

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
   (file:has-content "/etc/apt/apt.conf.d/02periodic" #>>~EOF>>
                     APT::Periodic::Enable "1";
                     APT::Periodic::Update-Package-Lists "1";
                     APT::Periodic::Download-Upgradeable-Packages "1";
                     APT::Periodic::Verbose "1";
                     EOF))
  (:unapply
   (file:does-not-exist "/etc/apt/apt.conf.d/02periodic")))

;; We might add keyword arguments so that the user could disable running the
;; cron daemon and configuring mailing root.
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
   (service-installed-running "cron")
   (desc "unattended-upgrades will mail root"
         (file:contains-lines "/etc/apt/apt.conf.d/50unattended-upgrades"
                              "Unattended-Upgrade::Mail \"root\";"))
   ;; work around Debian bug #812380
   (file:does-not-exist "/etc/apt/apt.conf.d/50unattended-upgrades.ucf-dist")
   :unapply (removed "unattended-upgrades")))

(defprop mirrors :posix (&rest uris)
  (:desc (format nil "apt mirror~P ~{~A~^, ~} selected" (length uris) uris))
  (:hostattrs
   (pushnew-hostattrs :apt.mirrors uris)))

(defpropspec uses-parent-mirrors :posix ()
  (:desc #?"Uses parent's apt mirror(s), if any")
  (aand (get-parent-hostattrs :apt.mirrors) `(mirrors ,@it)))

(defproplist proxy :posix (uri)
  (:desc #?"${uri} apt proxy selected")
  (:hostattrs (pushnew-hostattr :apt.proxy uri))
  (file:exists-with-content "/etc/apt/apt.conf.d/20proxy"
    (format nil "Acquire::HTTP::Proxy \"~A\";~%" uri)))

(defproplist uses-parent-proxy :posix ()
  (:desc #?"Uses parent's apt proxy")
  (proxy (or (get-parent-hostattrs-car :apt.proxy)
             (failed-change "Parent has no apt proxy"))))

(defproplist uses-local-cacher :posix ()
  (:desc "apt uses local apt cacher")
  (service-installed-running "apt-cacher-ng")
  (proxy "http://[::1]:3142"))

(defun get-mirrors ()
  (or (get-hostattrs :apt.mirrors)
      (get-default-mirrors (get-hostattrs-car :os))))

(defmethod get-default-mirrors ((os os:debian))
  '("http://deb.debian.org/debian"))

(defprop standard-sources.list :posix ()
  (:desc "Standard sources.list")
  (:apply
   ;; Migration: if we are about to truncate sources.list and create the
   ;; new sources.list.d/VENDOR.sources, first disable all old apt sources.
   ;; This avoids apt failing due to conflicting Signed-By for a suite.
   (when (and (remote-exists-every-p "/etc/apt/sources.list"
                                     "/etc/apt/sources.list.d")
              (plusp
               (nth-value 1 (remote-file-stats "/etc/apt/sources.list"))))
     (mrun "mv" "/etc/apt/sources.list.d" "/etc/apt/sources.list.d.old")
     (file:directory-exists "/etc/apt/sources.list.d"))

   (let ((os (get-hostattrs-car :os)))
     (prog-changes
       (add-change (file:has-content "/etc/apt/sources.list" ""))
       (add-change (file:exists-with-content
                       (format nil "/etc/apt/sources.list.d/~A.sources"
                               (etypecase os
                                 (os:debian "debian")))
                     (standard-sources-for os)))))))

(defmethod standard-sources-for ((os os:debian))
  (flet ((secp (suite)
           (string-suffix-p suite "-security"))
         (outlen (list)
           (reduce (lambda (a n) (+ a (1+ (length n)))) list
                   :initial-value 0)))
    (let* ((suite (os:debian-suite os))
           (old-suite-p (memstr= suite '("stretch" "buster")))
           (additional-suites (reverse (get-hostattrs 'additional-suites)))
           (archive-suites
             (delete-duplicates
              `(,suite
                ,@(and (subtypep (type-of os) 'os:debian-stable)
                       `(,#?"${suite}-updates"))
                ,@(and (subtypep (type-of os) 'os:debian-stable)
                       (not old-suite-p)
                       `(,#?"${suite}-backports"))
                ,@(remove-if #'secp additional-suites))
              :test #'string= :from-end t))
           (security-suites
             `(,@(and (or (subtypep (type-of os) 'os:debian-stable)
                          (subtypep (type-of os) 'os:debian-testing))
                      (list (if old-suite-p
                                #?"${suite}/updates"
                                #?"${suite}-security")))
               ,@(remove-if-not #'secp additional-suites))))
      (format nil #>>~EOF>>
              Types: deb deb-src
              URIs: ~{~A~^ ~}
              Suites:~:[~{ ~A~}~;~{~% ~A~}~]
              Components: main contrib non-free-firmware non-free
              Signed-By: /usr/share/keyrings/debian-archive-keyring.gpg~:[~;

              Types: deb deb-src
              URIs: http://security.debian.org/debian-security
              Suites:~:[~{ ~A~}~;~{~% ~A~}~]
              Components: main contrib non-free-firmware non-free
              Signed-By: /usr/share/keyrings/debian-archive-keyring.gpg~]
              EOF
              (get-mirrors)
              (> (outlen archive-suites) 63)
              archive-suites
              security-suites
              (> (outlen security-suites) 63)
              security-suites))))

(defprop additional-suites :posix (&rest suites)
  "Requests SUITES as additional apt suites that should be available.
Be sure to also apply APT:STANDARD-SOURCES.LIST to effect the request."
  (:hostattrs (os:required 'os:debianlike)
              (push-hostattrs 'additional-suites suites)))

(defpropspec additional-sources :posix (basename content)
  "Add additional apt source lines to a file in /etc/apt/sources.list.d named
after BASENAME.  CONTENT is as the content argument to FILE:HAS-CONTENT."
  (declare (indent 1))
  (:desc #?"Additional apt sources '${basename}' installed")
  (let ((deb822 (notany #~/^\S*deb(?:-src)? / (etypecase content
                                                (string (lines content))
                                                (list content))))
        (list-name #?"/etc/apt/sources.list.d/${basename}.list")
        (deb822-name #?"/etc/apt/sources.list.d/${basename}.sources"))
    `(on-change (with-unapply
                  (file:does-not-exist ,(if deb822 list-name deb822-name))
                  (file:exists-with-content ,(if deb822 deb822-name list-name)
                    ,content)
                  :unapply (file:does-not-exist ,list-name ,deb822-name))
       (updated))))

(defprop cache-cleaned :posix ()
  "Empty apt's cache to recover disk space."
  (:desc "apt cache cleaned")
  (:hostattrs (os:required 'os:debianlike))
  (:apply (apt-get "clean") :no-change))

(defproplist trusts-key :posix
    (fingerprint &optional (basename (remove #\Space fingerprint))
                 &aux (file #?"/etc/apt/trusted.gpg.d/${basename}.asc"))
  "Have apt trust the PGP key identified by FINGERPRINT to sign apt archives.
This facilty is deprecated in Debian trixie's apt.  Prefer supplying to
APT:ADDITIONAL-SOURCES file text with Signed-By fields pointing to files under
/etc/apt/keyrings.  Use FILE:DATA-UPLOADED to install the keys."
  (:desc #?"apt trusts PGP public key ${fingerprint}")
  (with-unapply
    (file:data-uploaded "--pgp-pubkey" (remove #\Space fingerprint) file)
    :unapply (file:does-not-exist file)))

(defproplist no-pdiffs :posix ()
  "Disable the use of PDiffs for machines with high bandwidth connections."
  (file:exists-with-content "/etc/apt/apt.conf.d/20pdiffs"
                            '("Acquire::PDiffs \"false\";")))


;;;; Pinning

(defmethod suite-specifier-to-os ((suite-specifier os:debian))
  suite-specifier)

(defmethod suite-specifier-to-os ((suite-specifier cons))
  (destructuring-bind (type &optional (suite nil suite-supplied-p))
      suite-specifier
    (apply #'make-instance type (and suite-supplied-p `(:suite ,suite)))))

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
  "Where PAIRS is a list of even length of alternating Debian suite specifiers
and apt pin priorities, request an apt source for the specified Debian suite
and pin that suite to a given pin value (see apt_preferences(5)).
Unapply to stop requesting the source and unpin the suite.

To effect the requests for the sources, also apply APT:STANDARD-SOURCES.LIST.

A Debian suite specifier is either an instance of a subclass of OS:DEBIANLIKE
or a list (TYPE &optional SUITE) where TYPE names a subclass of OS:DEBIANLIKE
and SUITE is a string.  For example, '(os:debian-stable \"bullseye\")."
  (:desc (loop for (os pin) on pairs by #'cddr
               for suite = (os:debian-suite (suite-specifier-to-os os))
               collect #?{Debian "${suite}" pinned, priority ${pin}}
                 into accum
               finally (return (format nil "~{~A~^; ~}" accum))))
  (:hostattrs (os:required 'os:debian))
  (loop for (suite-specifier pin) on pairs by #'cddr
        for os = (suite-specifier-to-os suite-specifier)
        for suite = (os:debian-suite os)
        for pref-file = #?"/etc/apt/preferences.d/20${suite}.pref"
        for cleanup = `(unapplied (additional-sources ,suite ""))
        do (check-type pin integer)
        collect `(file:exists-with-content ,pref-file
                   ,(suite-pin-block "*" os pin))
          into apply
        collect cleanup into apply collect cleanup into unapply
        collect `(additional-suites ,suite) into apply
        collect `(file:does-not-exist ,pref-file) into unapply
        finally (return `(with-unapply ,@apply :unapply ,@unapply))))

(defpropspec pinned :posix (preferences &rest pairs)
  "Pins a list of packages, package wildcards and/or regular expressions,
PREFERENCES, to a list of suites and corresponding pin priorities.  Unapply to
unpin.  PAIRS is a list of even length of alternating Debian suite specifiers
and apt pin priorities.

A Debian suite specifier is either an instance of a subclass of OS:DEBIANLIKE
or a list (TYPE &optional SUITE) where TYPE names a subclass of OS:DEBIANLIKE
and SUITE is a string.  For example, '(os:debian-stable \"bullseye\").

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
    (apt:suites-available-pinned '(os:debian-testing)  -10
                                 '(os:debian-unstable) -10)
    (apt:pinned '(\"elpa-*\")
                '(os:debian-testing)  100
                '(os:debian-unstable) 50)"
  (:desc (loop for (os pin) on pairs by #'cddr
               for suite = (os:debian-suite (suite-specifier-to-os os))
               collect #?{Debian "${suite}", priority ${pin}} into accum
               finally (return (format nil "~{~A~^, ~} pinned to ~{~A~^; ~}"
                                       preferences accum))))
  (:hostattrs (os:required 'os:debian))
  `(eseqprops
    ,@(loop for preference in preferences
            collect (list
                     'file:exists-with-content
                     (strcat "/etc/apt/preferences.d/10consfig_"
                             (string-to-filename preference)
                             ".pref")
                     (nbutlast
                      (loop for (os pin) on pairs by #'cddr
                            nconc (suite-pin-block
                                   preference (suite-specifier-to-os os) pin)
                            collect ""))))))


;;;; Reports on installation status

(defun apt-cache-policy (packages)
  (runlines :env '(:LANG "C") "apt-cache" "policy" packages))

(define-constant apt-cache-policy-installed #?/^\s+Installed:\s+(?!\(none\))/
  :test #'string=)

(defun all-installed-p (&rest packages)
  "Return true if all of PACKAGES, a list of non-virtual packages, are
installed."
  (loop with n = 0
        with packages* = (flatten packages)
        for line in (apt-cache-policy packages*)
        when (re:scan apt-cache-policy-installed line)
          do (incf n)
        finally (return (= n (length packages*)))))

(defun none-installed-p (&rest packages)
  "Returns true if none of PACKAGES, a list of non-virtual packages, are
installed."
  (loop for line in (apt-cache-policy (flatten packages))
        never (re:scan apt-cache-policy-installed line)))


;;;; Utilities

(defun apt-get (&rest args)
  (apply #'run :env +noninteractive-env+
         "apt-get" "-o" #?"DPkg::Lock::Timeout=${+dpkg-lock-timeout+}" args))
