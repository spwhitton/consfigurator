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

;;; Use of findmnt(1) makes much of this Linux-specific.

;;;; Methods on volumes to get strings for fstab

(defun get-findmnt-field (mountpoint field)
  (stripln (run "findmnt" "-nro" field mountpoint)))

(defmethod fs-spec ((volume filesystem))
  "Default implementation: no known source.
Other properties might fill it in."
  "none")

(defmethod fs-spec ((volume mounted-ext4-filesystem))
  (strcat "UUID=" (get-findmnt-field (mount-point volume) "UUID")))

(defmethod fs-spec ((volume mounted-fat32-filesystem))
  (strcat "PARTUUID=" (get-findmnt-field (mount-point volume) "PARTUUID")))

(defmethod fs-file ((volume filesystem))
  (mount-point volume))

(defmethod fs-vfstype ((volume ext4-filesystem))
  "ext4")

(defmethod fs-vfstype ((volume fat32-filesystem))
  "vfat")

(defmethod fs-mntops ((volume filesystem))
  (or (mount-options volume) '("none")))

(defmethod fs-freq ((volume filesystem))
  0)

(defmethod fs-passno ((volume filesystem))
  (if (eql #P"/" (mount-point volume))
      1 2))

(defmethod volume->entry ((volume filesystem))
  (format nil "~A ~A ~A ~{~A~^,~} ~A ~A"
          (fs-spec volume) (fs-file volume)
          (fs-vfstype volume) (fs-mntops volume)
          (fs-freq volume) (fs-passno volume)))


;;;; Properties

(defun entry->source (entry)
  (car (split-string entry)))

(defun entry->mountpoint (entry)
  (cadr (remove "" (split-string entry) :test #'string=)))

(defprop entries :posix (&rest entries)
  "Ensure that /etc/fstab contains each of ENTRIES, using a simple merge
procedure: existing lines of the fstab with the same mount point as any of
ENTRIES are updated to match the corresponding members of ENTRIES, except that
if the first field of the existing entry is not \"none\" and the corresponding
member of ENTRIES is \"none\", or \"PLACEHOLDER\", use the existing field value.

This makes it easy to update mount options without having to specify the
partition or filesystem UUID in your consfig."
  (:desc (format nil "fstab entries for ~{~A~^, ~}"
                 (mapcar #'entry->mountpoint entries)))
  (:apply
   (file:map-file-lines
    #P"/etc/fstab"
    (lambda (lines)
      (let ((pending (make-hash-table :test #'equal)))
        (dolist (entry entries)
          (setf (gethash (entry->mountpoint entry) pending) entry))
        (loop for line in lines
              for line-source = (entry->source line)
              and line-mountpoint = (entry->mountpoint line)
              for entry = (let ((entry (gethash line-mountpoint pending)))
                            (if (and (member (entry->source entry)
                                             '("none" "PLACEHOLDER")
                                             :test #'string=)
                                     (not (string= line-source "none")))
                                (format nil "~A ~{~A~^ ~}"
                                        line-source (cdr (split-string entry)))
                                entry))
              if entry
                collect it into accum and do (remhash line-mountpoint pending)
              else collect line into accum
              finally (return (nconc accum (hash-table-values pending)))))))))

(defprop entries-for-volumes :posix ()
  "Add or update entries in /etc/fstab for the host's volumes, as specified with
DISK:HAS-VOLUMES."
  (:desc "fstab entries for host's volumes")
  (:hostattrs (os:required 'os:linux))
  (:apply (apply #'entries
                 (mapcar #'volume->entry
                         (mapcan (curry #'subvolumes-of-type 'filesystem)
                                 (get-hostattrs :volumes))))))

(defprop entries-for-opened-volumes :posix ()
  "Add or update entries in /etc/fstab for currently open volumes.

This is used when building disk images and installing operating systems."
  (:desc "fstab entries for opened volumes")
  (:hostattrs (os:required 'os:linux))
  (:apply
   (apply #'entries
          (mapcar #'volume->entry
                  (mapcan (curry #'subvolumes-of-type 'mounted-filesystem)
                          (get-connattr :opened-volumes))))))
