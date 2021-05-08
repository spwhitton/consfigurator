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

(in-package :consfigurator.property.u-boot)
(named-readtables:in-readtable :consfigurator)

;; Currently we have a distinct property for each (Debian-specific)
;; installation script.  Perhaps there is some sensible parameterisation of
;; these available instead.

(defmethod install-bootloader-propspec
    ((type (eql 'u-boot-install-rockchip)) volume running-on-target
     &rest args &key &allow-other-keys)
  `(u-boot-installed-rockchip ,volume ,running-on-target ,@args))

(defmethod install-bootloader-binaries-propspec
    ((type (eql 'u-boot-install-rockchip)) volume &key &allow-other-keys)
  '(os:etypecase
       (debianlike
        (apt:installed "u-boot-rockchip"))))

(defprop u-boot-installed-rockchip :posix
    (volume running-on-target &key target)
  (:desc "Installed U-Boot using Debian scripts")
  (:hostattrs
   (os:required 'os:debianlike)
   (or running-on-target target
       (inapplicable-property
        "Must specify TARGET for u-boot-install-rockchip(8) unless running on device.")))
  (:apply
   (declare (ignore running-on-target))
   (let ((args (list "u-boot-install-rockchip" (device-file volume))))
     (if target
         (apply #'mrun :env `(:TARGET ,target) args)
         (apply #'mrun args)))))
