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

(in-package :consfigurator.property.crypttab)
(named-readtables:in-readtable :consfigurator)

;;;; Methods on volumes to get strings for crypttab

(defun get-lsblk-field (device field)
  (let ((val (stripln (run "lsblk" "-ndo" field device))))
    (if (string= val "") nil val)))

(defun get-device-parent (device)
  (multiple-value-bind (match groups)
      (re:scan-to-strings #?/^1\s+dependencies\s*:\s*\((\S+)\)$/
                          (run "dmsetup" "deps" "-o" "blkdevname" device))
    (and match (merge-pathnames (elt groups 0) #P"/dev/"))))

(defmethod ct-target ((volume opened-luks-container))
  (volume-label volume))

(defmethod ct-source ((volume opened-luks-container))
  (with-slots (device-file) volume
    (let ((parent
            (or (get-device-parent device-file)
                (failed-change
                 "Could not determine parent device of ~A" device-file))))
      (if-let ((partuuid (get-lsblk-field parent "PARTUUID")))
        (strcat "PARTUUID=" partuuid)
        (if-let ((uuid (get-lsblk-field parent "UUID")))
          (strcat "UUID=" uuid)
          (failed-change
           "Could not determine crypttab source field for ~A" device-file))))))

(defmethod ct-keyfile ((volume opened-luks-container))
  (if (slot-boundp volume 'crypttab-keyfile)
      (crypttab-keyfile volume)
      "none"))

(defmethod ct-options ((volume opened-luks-container))
  (or (crypttab-options volume) '("none")))

(defmethod volume-to-entry ((volume opened-luks-container))
  (format nil "~A ~A ~A ~{~A~^,~}"
          (ct-target volume) (ct-source volume)
          (ct-keyfile volume) (ct-options volume)))


;;;; Properties

(defprop has-entries :posix (&rest entries)
  "Ensure that /etc/crypttab contains each of ENTRIES, using a simple merge
procedure: existing lines of the crypttab with the same mapped device name as
any of ENTRIES are updated to match the corresponding members of ENTRIES,
except that if the second field of the existing entry is not \"none\" and the
corresponding member of ENTRIES is \"none\" or \"PLACEHOLDER\", use the
existing field value."
  (:desc
   (alet (loop for entry in entries collect (car (split-string entry)))
     (format nil "crypttab entr~@P for ~{~A~^, ~}" (length it) it)))
  (:apply (file:update-unix-table #P"/etc/crypttab" 1 0 entries)))

(defprop has-entries-for-opened-volumes :posix ()
  "Add or update entries in /etc/crypttab for currently open volumes.

This is used when building disk images and installing operating systems."
  (:desc "crypttab entries for opened volumes")
  (:hostattrs (os:required 'os:linux))
  (:apply
   (apply #'has-entries
          (mapcar #'volume-to-entry
                  (mapcan (curry #'subvolumes-of-type 'opened-luks-container)
                          (get-connattr :opened-volumes))))))
