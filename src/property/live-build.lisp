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

(in-package :consfigurator.property.live-build)
(named-readtables:in-readtable :consfigurator)

(defproplist installed :posix ()
  "Install the Debian Live tool suite.  See live-build(7)."
  (:desc "Debian Live live-build installed")
  (os:etypecase
    (debianlike (apt:installed "live-build"))))

(defun auto/config (config)
  (with-output-to-string (s)
    (format s "#!/bin/sh~%lb config noauto")
    (dolist (argument config)
      (princ " " s)
      (princ (escape-sh-token argument) s))
    (princ " \"${@}\"" s)
    (terpri s)))

(defprop %lbconfig :posix (dir)
  (:desc (declare (ignore dir))
         "lb config")
  (:apply
   (with-remote-current-directory (dir)
     (run :inform "lb" "config"))))

(defprop %lbbootstrap :posix
    (config-changed dir &aux (chroot (merge-pathnames "chroot/" dir)))
  (:desc (declare (ignore config-changed chroot))
         "lb bootstrap")
  (:hostattrs
   (declare (ignore config-changed chroot))
   (os:required 'os:linux))
  (:check
   (and (not config-changed)
        (remote-exists-p chroot)
        (not (remote-exists-p (merge-pathnames "debootstrap/" chroot)))))
  (:apply
   (declare (ignore config-changed))
   (ignoring-hostattrs (mount:unmounted-below-and-removed chroot))
   (with-remote-current-directory (dir)
     (apply #'run :inform
            (if-let ((proxy (get-hostattrs-car :apt.proxy)))
              (list :env (list :http_proxy proxy) "lb" "bootstrap")
              '("lb" "bootstrap"))))))

(defprop %lbchroot :posix (dir)
  (:desc (declare (ignore dir))
         "lb chroot")
  (:apply
   (with-remote-current-directory (dir)
     (run "lb" "chroot" "--force"))))

(defprop %lbbinary :posix (dir)
  (:desc (declare (ignore dir))
         "lb binary")
  (:apply
   (with-remote-current-directory (dir)
     (run :inform "lb" "binary" "--force"))))

(defpropspec image-built :lisp (config dir properties)
  "Build an image under DIR using live-build(7), where the resulting live
system has PROPERTIES, which should contain, at a minimum, a property from
CONSFIGURATOR.PROPERTY.OS setting the Debian suite and architecture.  CONFIG
is a list of arguments to pass to lb_config(1), not including the '-a' and
'-d' options, which Consfigurator will supply based on PROPERTIES.

This property runs the lb_config(1), lb_bootstrap(1), lb_chroot(1) and
lb_binary(1) commands to build or rebuild the image.  Rebuilding only occurs
when changes to CONFIG or PROPERTIES mean that the image is potentially
out-of-date; e.g. if you just add some new items to PROPERTIES then in most
cases only lb_chroot(1) and lb_binary(1) will be re-run.

Note that lb_chroot(1) and lb_binary(1) both run after applying PROPERTIES,
and might undo some of their effects.  For example, to configure
/etc/apt/sources.list, you will need to use CONFIG not PROPERTIES."
  (:desc (declare (ignore config properties))
         #?"Debian Live image built in ${dir}")
  (let* ((dir (ensure-directory-pathname dir))
         (chroot (merge-pathnames "chroot/" dir))
         (auto/config (merge-pathnames "auto/config" dir))
         (clean (mapcar (rcurry #'merge-pathnames
                                (merge-pathnames "config/" dir))
                        '("binary" "bootstrap" "chroot" "common" "source")))
         (host (chroot:make-child-host-for-chroot-deploy properties))
         (host-os (car (getf (hostattrs host) :os))))
    (when-let ((mirror (get-hostattrs-car :apt.mirror)))
      (setq config (list* "-m" mirror config)))
    (setq config (list* "-a" (os:debian-architecture host-os)
                        "-d" (os:debian-suite host-os) config))
    `(eseqprops
      (installed)
      (file:directory-exists ,(merge-pathnames "auto/" dir))
      (on-change (eseqprops
                  (on-change
                      (on-change
                          (file:has-content ,auto/config
                            ,(auto/config config) :mode #o755)
                        (file:does-not-exist ,@clean)
                        (%lbconfig ,dir))
                    (%lbbootstrap t ,dir))
                  (%lbbootstrap nil ,dir)
                  (deploys ((:chroot :into ,chroot)) ,host))
        ;; We could run lb_chroot before DEPLOYS, but lb_binary resets things
        ;; like /etc/apt/sources.list too, so doing that wouldn't avoid the
        ;; problem that sometimes CONFIG must be used when you'd normally use
        ;; PROPERTIES.  And we can't really determine whether or not lb_chroot
        ;; made a change, so it is not good for running inside the first
        ;; argument to ON-CHANGE.
        (%lbchroot ,dir)
        (%lbbinary ,dir)))))
