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

(in-package :consfigurator.property.mount)
(named-readtables:in-readtable :consfigurator)

(defprop mounted :posix (&key target)
  "Ensures that TARGET, a mount point configured in /etc/fstab, is mounted.
Mainly useful as a dependency of properties which might do the wrong thing if
the mount is not actually active."
  (:desc #?"${target} mounted")
  (:hostattrs (os:required 'os:linux))
  (:check (zerop (mrun :for-exit "findmnt" target)))
  (:apply (assert-euid-root)
          (file:directory-exists target)
          (mrun "mount" target)))

(defprop unmounted-below :posix (dir &key (and-at t))
  "Unmount anything mounted below DIR, and when AND-AT, anything mounted at DIR.

Not aware of shared subtrees, so you might need to use the --make-rslave
option to mount(8) first.  For example, if you did 'mount --rbind /dev
chroot/dev' then unless you also execute 'mount --make-rslave chroot/dev',
this property will empty /dev, breaking all kinds of things."
  (:desc #?"${dir} unmounted")
  (:hostattrs
   ;; findmnt(8) & --recursive argument to umount(8) are from util-linux
   (os:required 'os:linux))
  (:apply
   (with-change-if-changes-file-content ("/proc/mounts")
     ;; We used to call --make-rslave as we worked through, but for mounts
     ;; which were *not* made using the --rbind option to mount(8) or similar,
     ;; doing that can can get us into a state where we can unmount everything
     ;; we can see under DIR but the kernel will still consider the block
     ;; device to be in use.  That's a bit much for this property to deal
     ;; with, so we require that the caller call --make-rslave as appropriate.
     ;;
     ;; In addition to that problem, we would also require multiple unmount
     ;; passes; for example if we
     ;;
     ;;   % mount --bind chroot chroot
     ;;   % mount proc chroot/proc -t proc
     ;;   % mount --make-rslave chroot
     ;;
     ;; then we would need
     ;;
     ;;   % umount chroot/proc
     ;;   % umount chroot
     ;;   % umount chroot/proc
     ;;
     ;; because the --make-rslave leaves us with two independent mounts of
     ;; /proc, and the second can't be removed until the bind mount is
     ;; removed.  (This situation arises because :CHROOT.FORK connections bind
     ;; mount the chroot on itself if it is not already a mount point.)
     (loop with sorted
             = (if and-at
                   (all-mounts dir)
                   (delete (ensure-directory-pathname dir) (all-mounts dir)
                           :test #'pathname-equal))
           as next = (pop sorted)
           while next
           do (loop while (subpathp (car sorted) next) do (pop sorted))
              (mrun "umount" "--recursive" next)))))

(defprop unmounted-below-and-removed :posix (dir)
  "Unmount anything mounted below DIR, recursively delete the contents of DIR,
and unless DIR is itself a mount point, also remove DIR."
  (:desc #?"${dir} unmounted below and emptied/removed")
  (:hostattrs (os:required 'os:linux))
  (:check (or (not (remote-exists-p dir))
              (and (remote-mount-point-p dir)
                   (null (runlines "find" dir "-not" "-path" dir)))))
  (:apply (ignoring-hostattrs (unmounted-below dir :and-at nil))
          (if (remote-mount-point-p dir)
              (empty-remote-directory dir)
              (delete-remote-trees dir))))

(defun all-mounts (&optional (below #P"/"))
  "Retrieve all mountpoints below BELOW, ordered lexicographically.
If BELOW is itself a mountpoint, it will be included as the first element.

Uses findmnt(8), so Linux-specific."
  (let* ((below (ensure-directory-pathname below))
         (all-mounts (mapcar #'ensure-directory-pathname
                             (runlines "findmnt" "-rn" "--output" "target")))
         (mounts-below (remove-if-not (rcurry #'subpathp below) all-mounts)))
    (sort mounts-below #'string< :key #'unix-namestring)))


;;;; Utilities for :LISP properties

(defparameter *linux-basic-vfs* '(
("-t" "proc"     "-o" "nosuid,noexec,nodev"                "proc"   "/proc")
("-t" "sysfs"    "-o" "nosuid,noexec,nodev,ro"             "sys"    "/sys")
("-t" "devtmpfs" "-o" "mode=0755,nosuid"                   "udev"   "/dev")
("-t" "devpts"   "-o" "mode=0620,gid=5,nosuid,noexec"      "devpts" "/dev/pts")
("-t" "tmpfs"    "-o" "mode=1777,nosuid,nodev"             "shm"    "/dev/shm")
("-t" "tmpfs"    "-o" "mode=1777,strictatime,nodev,nosuid" "tmp"    "/tmp")))

(defparameter *linux-efivars-vfs*
  '("-t" "efivarfs" "-o" "nosuid,noexec,nodev" "efivarfs"
    "/sys/firmware/efi/efivars")
  "Arguments to mount(8) to mount the UEFI NVRAM.
After mounting /sys, mount this when /sys/firmware/efi/efivars exists.")

(defun assert-devtmpfs-udev-/dev ()
  "On a system with the Linux kernel, assert that /dev has fstype devtmpfs."
  (unless (and (remote-mount-point-p "/dev")
               (string= "devtmpfs udev"
                        (stripln (run "findmnt" "-nro" "fstype,source" "/dev"))))
    (failed-change
     "/dev is not udev devtmpfs; support for other kinds of /dev unimplemented.")))
