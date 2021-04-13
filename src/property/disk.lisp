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

(in-package :consfigurator.property.disk)
(named-readtables:in-readtable :consfigurator)

;;; All Linux-specific for now, so properties using these data types and
;;; functions should declare a hostattrs requirement (os:required 'os:linux).

;;;; Volumes

(defclass volume ()
  ((volume-label
    :initarg :volume-label
    :accessor volume-label
    :documentation "The name or label of the volume.
Can only be recorded in or on the volume itself for certain subclasses.  For
example, mostly meaningless for a Linux swap partition.")
   (volume-contents
    :initarg :volume-contents
    :accessor volume-contents)
;;    (volume-depth
;;     :initarg :volume-depth
;;     :accessor volume-depth
;;     :type integer
;;     :documentation
;;     "Number representing the degree to which this volume is nested within other
;; volumes.  All volumes with a lower value for VOLUME-DEPTH should be opened
;; before any attempt to open this volume is made.

;; For example, an LVM volume group needs a VOLUME-DEPTH strictly greater than
;; the VOLUME-DEPTH of all its physical volumes.")
   (volume-size
    :initarg :volume-size
    :accessor volume-size
    :documentation "The size of the volume, in whole mebibytes (MiB).
The special value :REMAINING means all remaining free space in the volume
containing this one.

If a larger size is required to accommodate the VOLUME-CONTENTS of the volume
plus any metadata (e.g. partition tables), this value will be ignored.")
   (volume-uuid
    :initarg :volume-uuid
    :accessor volume-uuid))
  (:documentation
   "Something which contains filesystems and/or other volumes."))

(defgeneric volume-contents-minimum-size (volume)
  (:documentation
   "Calculate the minimum size required to accomodate the contents of this volume."))

(defmethod volume-contents-minimum-size ((volume volume))
  (if (slot-boundp volume 'volume-contents)
      (reduce #'+ (mapcar #'volume-minimum-size
                          (ensure-cons (volume-contents volume))))
      0))

(defgeneric volume-minimum-size (volume)
  (:documentation
   "Return the VOLUME-SIZE of the volume or the minimum size required to
accommodate its contents, whichever is larger."))

(defmethod volume-minimum-size ((volume volume))
  (let ((volume-minimum-size
          (cond ((not (slot-boundp volume 'volume-size))
                 0)
                ((eql (volume-size volume) :remaining)
                 1)
                ((numberp (volume-size volume))
                 (volume-size volume))
                (t
                 (simple-program-error "Invalid volume size ~A"
                                       (volume-size volume))))))
    (max volume-minimum-size
         (volume-contents-minimum-size volume))))

(defclass top-level-volume (volume) ()
  (:documentation
   "A volume which never appears as the VOLUME-CONTENTS of another volume."))

(defgeneric open-volume-contents (volume file)
  (:documentation "Renders contents of VOLUME directly accessible.
FILE is something in the filesystem which serves as a means of accessing
VOLUME, for types of VOLUME where that makes sense, and nil otherwise.
Returns a possibly-empty list of fresh OPENED-VOLUME values.

An operation which mounts a filesystem, or similar, does not yield access to
any further volumes, and so should return the empty list."))

(defmethod open-volume-contents ((volume volume) file)
  "Default implementation: do nothing and yield no newly accessible volumes."
  nil)

(defgeneric close-volume-contents (volume file)
  (:documentation
   "Inverse of OPEN-VOLUME-CONTENTS: `kpartx -d`, `cryptsetup luksClose`, etc.
Return values, if any, should be ignored."))

(defgeneric create-volume (volume file)
  (:documentation
   "Create VOLUME on or at FILE, for example creating a GPT partition table and
its partitions on a block device.  Returns VOLUME."))


;;;; Opened volumes

(defclass opened-volume ()
  ((opened-volume
    :type volume
    :initarg :opened-volume
    :reader opened-volume
    :documentation "The VOLUME object that was opened.")
   (device-file
    :type pathname
    :initarg :device-file
    :accessor device-file
    :documentation "File under /dev giving access to the opened volume."))
  (:documentation
   "A VOLUME object which has been made directly accessible as a block device."))

(defmethod volume-contents ((volume opened-volume))
  "The contents of an opened volume is the contents of the volume opened."
  (volume-contents (opened-volume volume)))

(defmethod open-volume-contents ((volume opened-volume) (file null))
  (open-volume-contents (opened-volume volume) (device-file volume)))

(defmethod close-volume-contents ((volume opened-volume) (file null))
  (close-volume-contents (opened-volume volume) (device-file volume)))

(defclass physical-disk (top-level-volume opened-volume) ()
  (:documentation
   "A physical disk drive attached to the machine, which always has a
corresponding block device in /dev available to access it.  Should be used for
whole disks, not partitions (e.g. /dev/sda, not /dev/sda1)."))

(defmethod opened-volume ((volume physical-disk))
  volume)

;;;; Disk images

(defclass disk-image (volume)
  ((image-file
    :initarg :image-file
    :accessor image-file)))


;;;; Raw disk images

(defclass raw-disk-image (disk-image) ()
  (:documentation
   "A raw disk image, customarily given an extension of .img, suitable for
directly writing out with dd(1)."))

;; kpartx(1) can operate directly upon raw disk images, and will also make the
;; whole disk image accessible as a loop device, so we could examine the type
;; of (volume-contents volume), and if we find it's PARTITIONED-VOLUME, we
;; could call (open-volume-contents (volume-contents volume) file) and cons an
;; instance of OPENED-VOLUME for the whole disk onto the front of it (from
;; partitions at /dev/mapper/loopNpM we can infer that /dev/loopN is the whole
;; disk).  But for simplicity and composability, just make the whole disk
;; image accessible at this step of the recursion.
(defmethod open-volume-contents ((volume raw-disk-image) (file pathname))
  (list (make-instance 'opened-volume
                       :opened-volume volume
                       :device-file
                       (ensure-pathname
                        (stripln (run "losetup" "--show" "-f" file))))))

(defmethod close-volume-contents ((volume raw-disk-image) (file pathname))
  (mrun "losetup" "-d" file))

(defmethod create-volume ((volume raw-disk-image) (file pathname))
  "Ensure that a raw disk image exists.  Will overwrite only regular files."
  (when (test "-L" file "-o" "-e" file "-a" "!" "-f" file)
    (failed-change "~A already exists and is not a regular file." file))
  ;; Here, following Propellor, we want to ensure that the disk image size is
  ;; a multiple of 4096 bytes, so that the size is aligned to the common
  ;; sector sizes of both 512 and 4096.  But since we currently only support
  ;; volume sizes in whole mebibytes, we know it's already aligned.
  (file:does-not-exist file)
  (mrun
   "fallocate" "-l" (format nil "~DM" (volume-minimum-size volume)) file))


;;;; Partitioned block devices and their partitions

;;; No support for MSDOS partition tables.

(defclass partitioned-volume (volume)
  ((volume-contents
    :type cons
    :documentation "A list of partitions."))
  (:documentation "A device with a GPT partition table and partitions."))

(defclass partition (volume)
  ((partition-typecode
    :initform #x8300
    :initarg :partition-typecode
    :accessor partition-typecode
    :documentation
    "The type code for the partition; see the --typecode option to sgdisk(1).
Either a two-byte hexadecimal number, or a string specifying the GUID.

On GNU/Linux systems, you typically only need to set this to a non-default
value in the case of EFI system partitions, in which case use #xEF00."))
  (:documentation "A GPT partition."))

(defmethod volume-contents-minimum-size ((volume partitioned-volume))
  "Add one mebibyte for the GPT metadata."
  (1+ (call-next-method)))

(defmethod open-volume-contents ((volume partitioned-volume) (file pathname))
  (let ((loopdevs (mapcar
                   (lambda (line)
                     (destructuring-bind (add map loopdev &rest ignore)
                         (split-string line)
                       (declare (ignore ignore))
                       (unless (and (string= add "add") (string= map "map"))
                         (failed-change
                          "Unexpected kpartx output ~A" line))
                       (ensure-pathname (strcat "/dev/mapper/" loopdev))))
                   (runlines "kpartx" "-avs" file))))
    (unless (= (length loopdevs) (length (volume-contents volume)))
      (mrun "kpartx" "-d" file)
      (failed-change
       "kpartx(1) returned ~A loop devices, but volume has ~A partitions."
       (length loopdevs) (length (volume-contents volume))))
    (loop for loopdev in loopdevs and partition in (volume-contents volume)
          collect (make-instance 'opened-volume
                                 :opened-volume partition
                                 :device-file loopdev))))

(defmethod close-volume-contents ((volume partitioned-volume) (file pathname))
  (mrun "kpartx" "-d" file))

(defmethod create-volume ((volume partitioned-volume) (file pathname))
  (mrun :inform "sgdisk" "--zap-all" file)
  (mrun :inform "sgdisk"
        (loop for partition in (volume-contents volume)
              for code = (partition-typecode partition)
              collect (strcat "--new=0:0:"
                              (if (eql (volume-size partition) :remaining)
                                  "0"
                                  (format nil "+~DM"
                                          (volume-minimum-size partition))))
              collect (strcat "--typecode=0:"
                              (etypecase code
                                (string code)
                                (integer (format nil "~X" code)))))
        file))


;;;; LVM

(defclass lvm-volume-group (top-level-volume)
  ((volume-label
    :documentation "The name of the VG, often starting with \"vg_\".")
   ;; (volume-depth
   ;;  :initform 3)
   (volume-contents
    :type cons
    :documentation "A list of objects of type LVM-LOGICAL-VOLUME."))
  (:documentation
   "An LVM volume group.  Typically specified as a top level volume in
DISK:HAS-VOLUMES, rather than as the VOLUME-CONTENTS of another volume."))

(defmethod open-volume-contents ((volume lvm-volume-group) (file null))
  (mrun "vgscan")
  (mrun "vgchange" "-ay" (volume-label volume))
  ;; return a list of OPENED-VOLUME for each logical volume
  )

(defclass lvm-logical-volume (volume)
  ((volume-label
    :documentation "The name of the LV, often starting with \"lv_\".")))

(defclass lvm-physical-volume (volume)
  ((volume-contents
    :type null
    :initform nil)
   (volume-group
    :type string
    :initarg :volume-group
    :accessor volume-group
    :documentation
    "The name of the LVM volume group to which this volume belongs."))
  (:documentation "An LVM physical volume.
We do not specify what logical volumes it contains."))


;;;; Filesystems

(defparameter *mount-below* ""
  "Prefix for all filesystem mount points.  Bound by functions to request that
filesystems be mounted relative to a different filesystem root, e.g. under a
chroot.  The dynamic binding should last until after the filesystems are
unmounted, since the actual mount point is not stored.")

(defclass filesystem (volume)
  ((mount-point
    :type pathname
    :initarg :mount-point
    :accessor mount-point))
  (:documentation
   "A block device containing a filesystem, which can be mounted."))

(defmethod open-volume-contents ((volume filesystem) (file pathname))
  (mrun "mount" file (strcat *mount-below* (mount-point volume)))
  nil)

(defmethod close-volume-contents ((volume filesystem) (file pathname))
  (mrun "umount" file))

(defclass ext4-filesystem (filesystem) ())

(defmethod create-volume ((volume ext4-filesystem) (file pathname))
  (mrun "mkfs.ext4" file (and (volume-label volume)
                              `("-L" ,(volume-label volume)))))

(defclass fat32-filesystem (filesystem) ())

(defmethod create-volume ((volume fat32-filesystem) (file pathname))
  (mrun "mkdosfs" "-F" "32" (and (volume-label volume)
                                 `("-n" ,(volume-label volume)))
        file))


;;;; Other volumes which can be made accessible as block devices

(defclass luks-container (volume)
  ((luks-type
    :type string
    :initform "luks"
    :initarg :luks-type
    :accessor luks-type
    :documentation
    "The value of the --type parameter to cryptsetup luksFormat.
Note that GRUB2 older than 2.06 cannot open the default LUKS2 format, so
specify \"luks1\" if this is needed.")))
;; TODO ^ is it the default?

(defmethod open-volume-contents ((volume luks-container) (file pathname))
  ;; cryptsetup luksOpen FILE <generated from FILE>
  ;; pass --label when luks2  (is '--type luks' 1 or 2?)
  )

(defmethod create-volume ((volume luks-container) (file pathname))
  ;; find the passphrase by requesting data
  ;; ("--luks-passphrase--HOSTNAME" . (volume-label volume))
  )

(defclass linux-swap (volume) ())

(defmethod create-volume ((volume linux-swap) (file pathname))
  (mrun "mkswap" file))


;;;; Properties

(defprop has-volumes :posix (&rest volumes)
  "Specify the non-removable volumes normally accessible to the kernel on this
host."
  (:desc (declare (ignore volumes))
         "Has specified volumes.")
  (:hostattrs
   (os:required 'os:linux)
   (apply #'push-hostattrs :volumes volumes)))
