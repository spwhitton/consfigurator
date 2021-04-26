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
   (if (test "-d" (merge-pathnames "debootstrap/"
                                   (ensure-directory-pathname root)))
       (progn (delete-remote-trees root) nil)
       (test "-d" root)))
  (:apply
   (let* ((os (car (getf (hostattrs host) :os)))
          (args (list (if (os:supports-arch-p (get-hostattrs-car :os)
                                              (os:linux-architecture os))
                          "debootstrap" "qemu-debootstrap")
                      (plist-to-cmd-args options)
                      (strcat "--arch=" (os:debian-architecture os))
                      (os:debian-suite os)
                      root)))
     (when-let ((proxy (get-hostattrs-car :apt.proxy)))
       (setq args (list* :env (list :http_proxy proxy) args)))
     (when-let ((mirror (get-hostattrs-car :apt.mirror)))
       (nconcf args (list mirror)))
     (apply #'run args))))

(defpropspec %os-bootstrapper-installed :posix (host)
  (:desc (declare (ignore host)) "OS bootstrapper installed")
  `(os:host-etypecase ,host
     (debian
      (os:etypecase
        (debianlike (apt:installed "debootstrap"))))))

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
    (make-child-host :hostattrs (hostattrs host)
                     :propspec (make-propspec
                                :systems (propspec-systems propspec)
                                :propspec `(service:without-starting-services
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
   (%make-child-host (replace-propspec-into-host host properties))))

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
   #?"Built chroot for ${(car (getf (hostattrs host) :hostname))} @ ${root}")
  (%os-bootstrapper-installed child-host*)
  (%os-bootstrapped options root child-host*)
  (consfigurator:deploys `((:chroot :into ,root)) child-host))

(defproplist os-bootstrapped :lisp (options root properties)
  "Bootstrap an OS into ROOT and apply PROPERTIES.
OPTIONS is a plist of values to pass to the OS-specific bootstrapping property."
  (:desc (declare (ignore options properties))
         #?"Built chroot @ ${root}")
  (os-bootstrapped-for options root (make-host :propspec properties)))
