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

(defproplist has-swap-file :posix
    (size &optional (location #P"/var/lib/swapfile"))
  "Add a swap file.  SIZE is the -l argument to fallocate(1).
Current implementation assumes a non-CoW filesystem; see NOTES in swapon(8)."
  (:desc #?"Has swapfile of size ${size}")
  (:hostattrs (os:required 'os:linux))
  (on-apply-change (%swapfile-exists size location)
    (cmd:single "swapon" location))
  (fstab:has-entries
   (strcat (unix-namestring location) " swap swap defaults 0 0")))
