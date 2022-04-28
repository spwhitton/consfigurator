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

(in-package :consfigurator.property.fstab)
(named-readtables:in-readtable :consfigurator)

;;; Use of findmnt(8) makes much of this Linux-specific.

;;;; Methods on volumes to get strings for fstab

(defun get-findmnt-field (mountpoint field)
  (stripln (run "findmnt" "-nro" field mountpoint)))

(defmethod fs-spec ((volume filesystem) parent)
  "Default implementation: no known source.
Other properties might fill it in."
  "none")

(defmethod fs-spec ((volume mounted-ext4-filesystem) parent)
  (strcat "UUID=" (get-findmnt-field (mount-point volume) "UUID")))

(defmethod fs-spec ((volume mounted-fat32-filesystem) (parent partition))
  (strcat "PARTUUID=" (get-findmnt-field (mount-point volume) "PARTUUID")))

;; Filesystems of any type directly contained within LVM LVs, mounted or not.
(defmethod fs-spec ((volume filesystem) (parent lvm-logical-volume))
  (with-slots (volume-label lvm-volume-group) parent
    (unix-namestring
     (merge-pathnames volume-label
                      (ensure-directory-pathname
                       (merge-pathnames lvm-volume-group #P"/dev/"))))))

(defmethod fs-file ((volume filesystem))
  (let* ((ns (unix-namestring (mount-point volume)))
         (length (length ns)))
    (if (and (> length 1) (char= #\/ (last-char ns)))
        (subseq ns 0 (1- (length ns)))
        ns)))

(defmethod fs-vfstype ((volume ext4-filesystem))
  "ext4")

(defmethod fs-vfstype ((volume fat32-filesystem))
  "vfat")

(defmethod fs-mntops ((volume filesystem))
  (or (mount-options volume) '("defaults")))

(defmethod fs-freq ((volume filesystem))
  0)

(defmethod fs-passno ((volume filesystem))
  (if (eql #P"/" (mount-point volume))
      1 2))

(defmethod volume-to-entry ((volume filesystem) parent)
  (format nil "~A ~A ~A ~{~A~^,~} ~A ~A"
          (fs-spec volume parent) (fs-file volume)
          (fs-vfstype volume) (fs-mntops volume)
          (fs-freq volume) (fs-passno volume)))


;;;; Properties

(defprop has-entries :posix (&rest entries)
  "Ensure that /etc/fstab contains each of ENTRIES, using a simple merge
procedure: existing lines of the fstab with the same mount point as any of
ENTRIES are updated to match the corresponding members of ENTRIES, except that
if the first field of the existing entry is not \"none\" and the corresponding
member of ENTRIES is \"none\", or \"PLACEHOLDER\", use the existing field value.

This makes it easy to update mount options without having to specify the
partition or filesystem UUID in your consfig."
  (:desc (alet (mapcar (compose #'cadr #'words) entries)
           (format nil "fstab entr~@P for ~{~A~^, ~}" (length it) it)))
  (:apply (file:update-unix-table #P"/etc/fstab" 0 1 entries)))

(defprop has-entries-for-volumes :posix (&optional volumes)
  "Add or update entries in /etc/fstab for VOLUMES, or the host's volumes, as
specified with DISK:HAS-VOLUMES."
  (:desc (format nil "fstab entries for ~:[~;host's ~]volumes" volumes))
  (:hostattrs (os:required 'os:linux))
  (:apply (apply #'has-entries
                 (apply #'mapcar #'volume-to-entry
                        (multiple-value-list
                         (multiple-value-mapcan
                          (curry #'subvolumes-of-type 'filesystem)
                          (or volumes (get-hostattrs :volumes))))))))

;; TODO This is broken for fat32 partitions.  MOUNTED-FAT32-FILESYSTEM objects
;; are pushed directly to the connattr by DISK:WITH-THESE-OPEN-VOLUMES, rather
;; than appearing within OPENED-PARTITION objects.  Then the call to
;; SUBVOLUMES-OF-TYPE here never finds any parents, returning as a second
;; value a list containing only NIL.  Thus the specialisers in the
;; implementation of FS-SPEC for MOUNTED-FAT32-FILESYSTEM are never satisfied.
(defprop has-entries-for-opened-volumes :posix ()
  "Add or update entries in /etc/fstab for currently open volumes.

This is used when building disk images and installing operating systems."
  (:desc "fstab entries for opened volumes")
  (:hostattrs (os:required 'os:linux))
  (:apply
   (apply #'has-entries
          (apply #'mapcar #'volume-to-entry
                 (multiple-value-list
                  (multiple-value-mapcan
                   (curry #'subvolumes-of-type 'mounted-filesystem)
                   (get-connattr :opened-volumes)))))))
