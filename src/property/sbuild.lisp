;;; Consfigurator -- Lisp declarative configuration management system

;;; Copyright (C) 2016, 2021  Sean Whitton <spwhitton@spwhitton.name>

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

(in-package :consfigurator.property.sbuild)
(named-readtables:in-readtable :consfigurator)

(defproplist installed :posix ()
  "Ensure that sbuild and associated utilities are installed."
  (:desc "sbuild and associated utilities installed")
  (os:etypecase
    (debianlike (apt:installed "piuparts" "autopkgtest" "lintian" "sbuild"))))

(defproplist usable-by :posix (username)
  "Add a user to the sbuild group in order to use sbuild."
  (:desc #?"sbuild usable by ${username}")
  (installed)
  (user:has-groups username "sbuild"))

(defproplist %sbuild-ccache-has-some-limits :posix ()
  "Set a default limit on the sbuild ccache, only if the ccache does not already
exist, so that the user can easily override this default."
  (:desc #?"Default limits on sbuild ccache")
  (:check (remote-exists-p "/var/cache/ccache-sbuild"))
  (ccache:has-limits "/var/cache/ccache-sbuild" :max-size "2Gi"))

(defpropspec built :lisp
    (options properties
             &aux (host
                   (make-child-host
                    :propspec
                    (append-propspecs
                     properties
                     (make-propspec
                      :systems nil
                      :propspec
                      '(desc "Build packages installed into chroot"
                        (os:etypecase
                          (debianlike (apt:installed "eatmydata" "ccache"))))))))
             (os (get-hostattrs-car :os host))
             (suite (os:debian-suite os))
             (arch (os:debian-architecture os)))
  "Build and configure a schroot for use with sbuild.
For convenience we set up several enhancements, such as ccache and eatmydata.
In the case of Debian, we assume you are building for Debian stretch or newer,
and we assume that you have sbuild 0.71.0 or later and, if overlays are
enabled, Linux 3.18 or newer.

OPTIONS is a plist of keyword parameters:

  - :USE-CCACHE -- whether builds using the schroot should use ccache.  ccache
    is generally useful but breaks building some packages; this option allows
    you to toggle it on and off for particular schroots.  Defaults to t.

  - :CHROOT-OPTIONS -- passed on to CHROOT:OS-BOOTSTRAPPED-FOR, which see.

PROPERTIES should specify, at a minimum, the operating system for the schroot.

Example usage:

    (os:debian-stable \"bullseye\" :amd64)
    (apt:uses-local-cacher)
    (apt:mirrors \"...\")
    (sbuild:usable-by \"spwhitton\")
    (schroot:overlays-in-tmpfs)
    (periodic:at-most :monthly \"sbuild sid schroot rebuilt\"
      (unapplied (sbuild:built. nil (os:debian-unstable :amd64))))
    (sbuild:built. nil
      (os:debian-unstable :amd64)
      (sbuild:standard-debian-schroot)
      (apt:uses-parent-proxy)
      (apt:uses-parent-mirrors))

To take advantage of the piuparts and autopkgtest support, add to your
~/.sbuildrc:

    $piuparts_opts = [
        '--no-eatmydata',
        '--schroot',
        '%r-%a-sbuild',
        '--log-level=info',
        ];

    $autopkgtest_root_args = \"\";
    $autopkgtest_opts = [\"--\", \"schroot\", \"%r-%a-sbuild\"];"
  (:desc (format nil "Built sbuild schroot for ~A/~A" suite arch))
  (destructuring-bind
      (&key (use-ccache t) chroot-options
       &aux
         (chroot-options (if (member :variant chroot-options)
                             chroot-options
                             (list* :variant "buildd" chroot-options)))
         (chroot
          (ensure-pathname
           (format nil "~A-~A" suite arch)
           :ensure-directory t :ensure-absolute t :defaults #P"/srv/chroot/"))
         (desc (format nil "~A/~A autobuilder" suite arch))
         (conf (ensure-pathname
                (format nil "~A-~A-sbuild-consfigurator" suite arch)
                :ensure-absolute t :defaults #P"/etc/schroot/chroot.d/")))
      options
    `(with-unapply
         (installed)

       ;; ccache
       ,@(and use-ccache '((%sbuild-ccache-has-some-limits)
                           (ccache:cache-for-group "sbuild")))
       (desc
        "ccache mounted in sbuild schroots"
        (file:contains-lines "/etc/schroot/sbuild/fstab"
          "/var/cache/ccache-sbuild /var/cache/ccache-sbuild none rw,bind 0 0"))
       ;; Script from <https://wiki.debian.org/sbuild>.
       (file:has-content "/var/cache/ccache-sbuild/sbuild-setup"
#>EOF>#!/bin/sh

export CCACHE_DIR=/var/cache/ccache-sbuild
export CCACHE_UMASK=002
export CCACHE_COMPRESS=1
unset CCACHE_HARDLINK
export PATH="/usr/lib/ccache:$PATH"

exec "$@"
EOF :mode #o755)

       ;; schroot
       (chroot:os-bootstrapped-for ,chroot-options ,chroot ,host)
       (desc
        ,(strcat "schroot configuration for " desc)
        (file:contains-ini-settings
         ,conf
         ,@(mapcar
            (curry #'cons (format nil "~A-~A-sbuild" suite arch))
            `(("description" ,desc)
              ("groups"      "root,sbuild")
              ("root-groups" "root,sbuild")
              ("profile"     "sbuild")
              ("type"        "directory")
              ("directory"   ,(drop-trailing-slash (unix-namestring chroot)))
              ,@(and (get-hostattrs-car 'schroot:uses-overlays)
                     `(("union-type" "overlay")))

              ;; If we're building a sid chroot, add useful aliases.  In order
              ;; to avoid more than one schroot getting the same aliases, we
              ;; only do this if the arch of the chroot equals the host arch.
              ,@(and (string= suite "unstable")
                     (string= arch (os:debian-architecture
                                    (get-hostattrs-car :os)))
                     `(("aliases"
                        ,(format
                          nil "~@{~A~^,~}"
                          "sid"
                          ;; If the user wants to build for experimental, they
                          ;; would use their sid chroot and sbuild's
                          ;; --extra-repository option to enable experimental.
                          "rc-buggy"
                          "experimental"
                          ;; We assume that building for UNRELEASED means
                          ;; building for unstable.
                          "UNRELEASED"
                          ;; The following is for dgit compatibility.
                          (strcat "UNRELEASED-"
                                  (os:debian-architecture os)
                                  "-sbuild")))))

              ("command-prefix"
               ,(if use-ccache
                    "/var/cache/ccache-sbuild/sbuild-setup,eatmydata"
                    "eatmydata"))))))

       ;; TODO We should kill any sessions still using the chroot before
       ;; destroying it (as suggested by sbuild-destroychroot(8)).
       :unapply
       (unapplied (chroot:os-bootstrapped-for ,chroot-options ,chroot ,host))
       (file:does-not-exist ,conf))))

;; Here we combine Propellor's Sbuild.osDebianStandard and Sbuild.update.
(defpropspec standard-debian-schroot :posix (&key (upgrade :weekly))
  "Properties that will be wanted in almost any Debian sbuild schroot, but not
in sbuild schroots for other operating systems.

Includes replacing use of sbuild-update(1)."
  (:desc "Standard Debian sbuild properties")
  `(eseqprops (apt:standard-sources.list)
              (periodic:at-most ,upgrade "sbuild schroot updated"
                (apt:updated) (apt:upgraded) (apt:autoremoved))))
