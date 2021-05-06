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

;;;; Bootloaders

(defgeneric install-bootloader (bootloader-type volume running-on-target &key)
  (:documentation "Install bootloader of type BOOTLOADER-TYPE to VOLUME.

RUNNING-ON-TARGET indicates whether the host to which we are connected is the
host the bootloader will boot.  For example, it is NIL when building disk
images, and T when installing a host from a live environment.  Bootloader
installation might behave differently when RUNNING-ON-TARGET is NIL, or error
out.

Only :LISP property :APPLY subroutines will call this function."))

(defgeneric install-bootloader-binaries (bootloader-type volume &key)
  (:documentation
   "Return a :POSIX propapp which fetches/installs whatever binaries/packages
need to be available to install BOOTLOADER-TYPE to VOLUME."))


;;;; Properties

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

(defprop %install-bootloaders :lisp (running-on-target)
  (:desc #?"Installed host bootloaders")
  (:apply
   (assert-euid-root)
   (dolist (volume (mapcan #'all-subvolumes (get-connattr :opened-volumes)))
    (when (slot-boundp volume 'volume-bootloader)
      (destructuring-bind (type . args) (volume-bootloader volume)
        (apply #'install-bootloader type volume running-on-target args))))))

(defpropspec chroot-installed-to-volumes :lisp
    (host chroot volumes &key running-on-target)
  "Where CHROOT contains the root filesystem of HOST and VOLUMES is a list of
volumes, recursively open the volumes and rsync in the contents of CHROOT.
Also update the fstab and crypttab, and try to install bootloader(s)."
  (:desc #?"${chroot} installed to volumes")
  (let ((target
          (ensure-directory-pathname
           (strcat
            (drop-trailing-slash
             (unix-namestring (ensure-directory-pathname chroot)))
            ".target"))))
    `(with-these-open-volumes (,volumes :mount-below ,target)
       (%update-target-from-chroot ,chroot ,target)
       ,(propapp
         (chroot:deploys-these. target host
           (os:etypecase
               (debianlike
                (file:lacks-lines "/etc/fstab"
                                  "# UNCONFIGURED FSTAB FOR BASE SYSTEM")
                ;; These will overwrite any custom mount options, etc., with
                ;; values from VOLUMES.  Possibly it would be better to use
                ;; properties which only update the fs-spec/source fields.
                ;; However, given that VOLUMES ultimately comes from the
                ;; volumes the user has declared for the host, it is unlikely
                ;; there are other properties setting mount options etc. which
                ;; are in conflict with VOLUMES.
                (fstab:entries-for-opened-volumes)
                (crypttab:entries-for-opened-volumes)))
           (%install-bootloaders running-on-target))))))

(defpropspec bootloader-binaries-installed :posix ()
  "Install whatever binaries/packages need to be available to install the host's
bootloaders to its volumes from within that host."
  (:desc #?"Bootloader binaries installed")
  (loop
    for volume in (mapcan #'all-subvolumes (get-hostattrs :volumes))
    when (slot-boundp volume 'volume-bootloader)
      collect (destructuring-bind (type . args) (volume-bootloader volume)
                (apply #'install-bootloader-binaries type volume args))
        into propapps
    finally
       (setq propapps (delete-duplicates propapps :test #'tree-equal))
       (return (if (cdr propapps) (cons 'eseqprops propapps) (car propapps)))))
