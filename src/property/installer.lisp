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

(in-package :consfigurator.property.installer)
(named-readtables:in-readtable :consfigurator)

(defprop %update-target-from-chroot :posix (chroot target)
  (:desc #?"Updated ${target} from ${chroot}")
  (:apply
   (assert-euid-root)
   (run "rsync" "-PSavx" "--delete"
        (loop for volume
                in (mapcan (curry #'subvolumes-of-type 'mounted-filesystem)
                           (get-connattr :opened-volumes))
              collect (strcat "--exclude="
                              (unix-namestring (mount-point volume))))
        (strcat (unix-namestring chroot) "/")
        (strcat (unix-namestring target) "/"))))

(defpropspec chroot-installed-to-volumes :lisp (host chroot volumes)
  "Where CHROOT contains the root filesystem of HOST and VOLUMES is a list of
volumes, recursively open the volumes and rsync in the contents of CHROOT.
Also update the fstab and crypttab, and try to install a bootloader."
  (:desc #?"${chroot} installed to volumes")
  (let ((target
          (ensure-directory-pathname
           (strcat
            (drop-trailing-slash
             (unix-namestring (ensure-directory-pathname chroot)))
            ".target"))))
    `(with-these-open-volumes (,volumes :mount-below ,target)
       (%update-target-from-chroot ,chroot ,target)
       (chroot:deploys-these
        ,target ,host
        ,(make-propspec
          :systems nil
          :propspec
          '(eseqprops
            (fstab:entries-for-opened-volumes)
            (file:lacks-lines "/etc/fstab" "# UNCONFIGURED FSTAB FOR BASE SYSTEM"))))
       ;; TODO Update /etc/crypttab
       ;; TODO Install bootloader
       )))
