;;; Consfigurator -- Lisp declarative configuration management system

;;; Copyright (C) 2021, 2025  Sean Whitton <spwhitton@spwhitton.name>

;;; This file is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3, or (at your option)
;;; any later version.

;;; This file is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.

;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

(in-package :consfigurator.property.swap)
(named-readtables:in-readtable :consfigurator)

(defprop %swapfile-exists :posix (size location)
  (:check
   (declare (ignore size))
   (remote-exists-p location))
  (:apply
   (mrun #?"umask 077; fallocate -l ${size} ${(unix-namestring location)}")
   (mrun "mkswap" location))
  (:unapply
   (declare (ignore size))
   (mrun :may-fail "swapoff" location)
   (delete-remote-trees location)))

(defproplist has-swap-file :posix (size &key (location #P"/var/lib/swapfile"))
  "Add a swap file.  SIZE is the -l argument to fallocate(1).
Current implementation assumes a non-CoW filesystem; see NOTES in swapon(8)."
  (:desc #?"Has swapfile of size ${size}")
  (:hostattrs (os:required 'os:linux))
  (on-apply-change (%swapfile-exists size location)
    (cmd:single "swapon" location))
  (fstab:has-entries
   (strcat (unix-namestring location) " swap swap defaults 0 0")))

(defproplist %resume-configured :posix (partition offset)
  (on-change
      (eseqprops
       (file:exists-with-content "/etc/default/grub.d/resume.cfg"
         `(,#?{GRUB_CMDLINE_LINUX_DEFAULT="$GRUB_CMDLINE_LINUX_DEFAULT \
               resume=${partition} resume_offset=${offset}"}))
       (file:exists-with-content "/etc/initramfs-tools/conf.d/resume"
         `(,#?"resume=${partition} resume_offset=${offset}")))
    (cmd:single "update-initramfs" "-u")
    (cmd:single "update-grub")))

(defprop resume-configured :posix (&key (swap-file #P"/var/lib/swapfile"))
  "Configure system to hibernate to and resume from SWAP-FILE.
Only configuring GRUB is currently implemented."
  (:desc "System resumes from ~A" swap-file)
  (:apply
   (unless (remote-test "-d" "/etc/default/grub.d")
     (aborted-change "/etc/default/grub.d not a directory"))
   (let ((device (or (#~#^/dev/\S+# (cadr (runlines "df" "-P" swap-file)))
                     (aborted-change "Failed to parse df(1) output")))
         (offset (or (#1~/^\s*0:\s+0\.\.\s+0:\s+(\d+)\.\./m
                      (run "filefrag" "-v" swap-file))
                     (aborted-change "Failed to parse filefrag(8) output"))))
     (%resume-configured (disk:get-partition-uuid device) offset))))
