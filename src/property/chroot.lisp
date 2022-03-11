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

(in-package :consfigurator.property.chroot)
(named-readtables:in-readtable :consfigurator)

(defprop %debootstrapped :posix (root host &rest options)
  "Bootstrap The Universal Operating System into ROOT using debootstrap(1)."
  (:check
   (declare (ignore options host))
   ;; check whether a previous debootstrap failed partway through
   (if (remote-test "-d" (merge-pathnames "debootstrap/"
                                          (ensure-directory-pathname root)))
       (progn (delete-remote-trees root) nil)
       (remote-exists-p (chroot-pathname "/usr/lib/os-release" root))))
  (:apply
   (destructuring-bind
       (&key (apt.proxy (get-hostattrs-car :apt.proxy host))
          (apt.mirror (get-hostattrs-car :apt.mirror host))
        &allow-other-keys
        &aux (os (get-hostattrs-car :os host))
          (args (list "debootstrap"
                      (plist->long-options
                       (remove-from-plist options :apt.proxy :apt.mirror))
                      (strcat "--arch=" (os:debian-architecture os))
                      (os:debian-suite os)
                      root)))
       options

     ;; In the case where the chroot arch is not equal to the host arch, we
     ;; could execute arch-test(1) here to confirm the architecture is
     ;; executable by the running kernel; we'd add arch-test alongside
     ;; qemu-user-static in %OS-BOOTSTRAPPER-INSTALLED.  Or possibly we only
     ;; try to execute arch-test(1) when we find it's already on PATH.

     (when apt.proxy
       (setq args (list* :env (list :http_proxy apt.proxy) args)))
     (when apt.mirror
       (nconcf args (list apt.mirror)))
     (apply #'run args))))

(defproplist %debootstrap-manually-installed :posix ()
  ;; Accept any debootstrap we find on path to enable installing Debian on
  ;; arbitrary unixes, where Consfigurator does not know how to install
  ;; packages, but the user has manually installed debootstrap(8).
  (:check (remote-executable-find "debootstrap"))
  (package:installed nil '(:apt ("debootstrap"))))

(defpropspec %os-bootstrapper-installed :posix (host)
  (:desc "OS bootstrapper installed")
  (let ((host (preprocess-host host)))
    `(os:host-etypecase ,host
       (debian
        (os:typecase
          (debianlike (apt:installed "debootstrap"))
          (t (%debootstrap-manually-installed)))
        ;; Don't have an escape hatch like the :CHECK subroutine of
        ;; %DEBOOTSTRAP-MANUALLY-INSTALLED for the case where the
        ;; architectures do not match because ensuring that debootstrap(8)
        ;; will be able to bootstrap a foreign arch is more involved.
        ,@(and (not (os:supports-arch-p
                     (get-hostattrs-car :os) (os:linux-architecture
                                              (get-hostattrs-car :os host))))
               '((os:etypecase
                   (debianlike (apt:installed "qemu-user-static")))))))))

(defpropspec %os-bootstrapped :posix (options root host)
  "Bootstrap OS into ROOT, e.g. with debootstrap(1)."
  ;; evaluate HOST once; can't use ONCE-ONLY because gensyms not serialisable
  ;; for sending to remote Lisp images
  (:desc (declare (ignore options root host)) "OS bootstrapped")
  (let ((host host))
    `(os:host-etypecase ,host
       (debian (%debootstrapped ,root ,host ,@options)))))

(defmethod %make-child-host ((host unpreprocessed-host))
  (let ((propspec (host-propspec host)))
    (make-child-host
     :hostattrs (hostattrs host)
     :propspec (make-propspec
                :systems (propspec-systems propspec)
                :propspec `(service:without-starting-services
                             (container:contained :filesystem)
                             ,(propspec-props propspec))))))

(defproplist deploys :lisp (root host &optional additional-properties)
  "Like DEPLOYS with first argument `((:chroot :into ,root)), but disable
starting services in the chroot, and set up access to parent hostattrs."
  (:desc #?"Subdeployment of ${root}")
  (consfigurator:deploys
   `((:chroot :into ,root))
   (%make-child-host (union-propspec-into-host host additional-properties))))

(defproplist deploys-these :lisp (root host properties)
  "Like DEPLOYS-THESE with first argument `((:chroot :into ,root)), but disable
starting services in the chroot, and set up access to parent hostattrs."
  (:desc #?"Subdeployment of ${root}")
  (consfigurator:deploys
   `((:chroot :into ,root))
   (%make-child-host
    (replace-propspec-into-host (ensure-host host) properties))))

(defproplist os-bootstrapped-for :lisp
    (options root host &optional additional-properties
             &aux
             (child-host (%make-child-host
                          (union-propspec-into-host host additional-properties)))
             (child-host* (preprocess-host child-host)))
  "Bootstrap an OS for HOST into ROOT and apply the properties of HOST.
OPTIONS is a plist of values to pass to the OS-specific bootstrapping property."
  (:desc
   (declare (ignore options))
   #?"Built chroot for ${(get-hostname child-host*)} @ ${root}")
  (with-unapply
    (%os-bootstrapper-installed child-host*)
    (%os-bootstrapped options root child-host*)
    (consfigurator:deploys `((:chroot :into ,root)) child-host)
    :unapply (mount:unmounted-below-and-removed root)))

(defproplist os-bootstrapped :lisp (options root properties)
  "Bootstrap an OS into ROOT and apply PROPERTIES.
OPTIONS is a plist of values to pass to the OS-specific bootstrapping property."
  (:desc (declare (ignore options properties))
         #?"Built chroot @ ${root}")
  (os-bootstrapped-for options root (make-host :propspec properties)))
