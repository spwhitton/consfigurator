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

(in-package :consfigurator.property.grub)
(named-readtables:in-readtable :consfigurator)

(defmethod install-bootloader-propspec
    ((type (eql 'grub)) volume running-on-target
     &rest args &key &allow-other-keys)
  `(grub-installed ,volume ,running-on-target ,@args))

(defmethod install-bootloader-binaries-propspec
    ((type (eql 'grub)) volume &key (target "i386-pc") &allow-other-keys)
  `(os:etypecase
       (debianlike
        (apt:installed
         "initramfs-tools"
         ,(eswitch (target :test #'string=)
            ("i386-pc" "grub-pc")
            ("x86_64-efi" "grub-efi-amd64"))))))

(defprop grub-installed :posix
    (volume running-on-target &key (target "i386-pc") force-extra-removable)
  "Use grub-install(8) to install grub to VOLUME."
  (:desc "GRUB installed")
  (:apply
   (assert-euid-root)
   (mrun :inform "update-initramfs" "-u")
   (let ((os-prober (and (not running-on-target)
                         (remote-exists-p "/etc/grub.d/30_os-prober"))))
     ;; work around Debian bug #802717
     (when os-prober (file:has-mode "/etc/grub.d/30_os-prober" #o644))
     (mrun :inform "update-grub")
     (when os-prober (file:has-mode "/etc/grub.d/30_os-prober" #o755)))
   (mrun :inform "grub-install" (strcat "--target=" target)
         (and (string-suffix-p target "-efi") (not running-on-target)
              "--no-nvram")
         (and force-extra-removable "--force-extra-removable")
         (device-file volume))))
