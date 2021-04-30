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

(defprop unmounted-below :posix (dir)
  "Unmount anything mounted at or below DIR.

Not aware of shared subtrees, so you might need to use the --make-rslave
option to mount(1) first.  For example, if you did 'mount --rbind /dev
chroot/dev' then unless you also execute 'mount --make-rslave chroot/dev',
this property will empty /dev, breaking all kinds of things."
  (:desc #?"${dir} unmounted")
  (:hostattrs
   ;; findmnt(1) & --recursive argument to umount(1) are from util-linux
   (os:required 'os:linux))
  (:apply
   (with-change-if-changes-file-content ("/proc/mounts")
     ;; We used to call --make-rslave as we worked through, but for mounts
     ;; which were *not* made using the --rbind option to mount(1) or similar,
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
     (let* ((dir (ensure-directory-pathname dir))
            (all-mounts
              (mapcar #'ensure-directory-pathname
                      (runlines "findmnt" "-rn" "--output" "target")))
            (mounts-below (remove-if-not (rcurry #'subpathp dir) all-mounts))
            (sorted (sort mounts-below #'string< :key #'unix-namestring)))
       (loop as next = (pop sorted)
             while next
             do (loop while (subpathp (car sorted) next) do (pop sorted))
                (mrun "umount" "--recursive" next))))))

(defproplist unmounted-below-and-removed :posix (dir)
  "Unmount anything mounted at or below DIR and recursively delete dir."
  (:desc #?"${dir} unmounted and removed")
  (:check (not (remote-exists-p dir)))
  (unmounted-below dir)
  (cmd:single "rm" "-rf" dir))
