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
    :type string
    :initarg :volume-label
    :accessor volume-label
    :documentation "The name or label of the volume.
Can only be recorded in or on the volume itself for certain subclasses.  For
example, mostly meaningless for a Linux swap partition.")
   (volume-contents
    :type volume
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
plus any metadata (e.g. partition tables), this value will be ignored."))
  (:documentation
   "Something which contains filesystems and/or other volumes."))

(defgeneric volume-contents-minimum-size (volume)
  (:documentation
   "Return the minimum size required to accommodate the VOLUME-CONTENTS of VOLUME."))

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

(defgeneric create-volume (volume file)
  (:documentation
   "Create VOLUME.  FILE is a pathname at or on which to create VOLUME, for types
of VOLUME where that makes sense, and explicitly nil otherwise.
Return values, if any, should be ignored."))


;;;; Opened volumes

(defclass opened-volume (volume)
  ((device-file
    :type pathname
    :initarg :device-file
    :accessor device-file
    :documentation "File under /dev giving access to the opened volume.")
   (volume-uuid
    :initarg :volume-uuid
    :accessor volume-uuid
    :documentation
    "A UUID for the volume, quoted and prefixed with the tag name, suitable for
insertion into files like /etc/crypttab and /etc/fstab.  E.g.

    PARTUUID=\"25164b50-4fbb-4ca0-ab01-a30a838bdf3b\""))
  (:documentation
   "A VOLUME object which has been made directly accessible as a block device."))

(defgeneric make-opened-volume (volume device-file)
  (:documentation
   "Where there is a class which is a subclass of both the class of VOLUME and
OPENED-VOLUME, make a fresh instance of that class copying all slots from
VOLUME, and setting the DEVICE-FILE slot to DEVICE-FILE."))

(defmacro defclass-opened-volume
    (name (subclass-of-volume &rest other-superclasses))
  "Define a subclass of SUBCLASS-OF-VOLUME and OPENED-VOLUME called NAME, and an
appropriate implementation of MAKE-OPENED-VOLUME for NAME.
SUBCLASS-OF-VOLUME should be a symbol naming a subclass of VOLUME."
  `(progn
     (defclass ,name (,subclass-of-volume ,@other-superclasses opened-volume) ()
       (:documentation
        ,(format
          nil
          "Instance of ~A which has been made directly accessible as a block device."
          name)))
     (defmethod make-opened-volume
         ((volume ,subclass-of-volume) (device-file pathname))
       ,(format nil "Make instance of ~A from instance of ~A."
                name subclass-of-volume)
       (let ((old-class (find-class ',subclass-of-volume)))
         (closer-mop:ensure-finalized old-class)
         (let ((new (allocate-instance (find-class ',name))))
           (dolist (slot-name (mapcar #'closer-mop:slot-definition-name
                                      (closer-mop:class-slots old-class)))
             (when (slot-boundp volume slot-name)
               (setf (slot-value new slot-name)
                     (slot-value volume slot-name))))
           (setf (slot-value new 'device-file) device-file)
           (reinitialize-instance new))))))

(defgeneric open-volume (volume file)
  (:documentation "Renders contents of VOLUME directly accessible.
FILE is something in the filesystem which serves as a means of accessing
VOLUME, for types of VOLUME where that makes sense, and explicitly nil
otherwise.

Returns as a first value a fresh instance of OPENED-VOLUME corresponding to
VOLUME.  In this case, it is legitimate to subsequently call OPEN-VOLUME on
the VOLUME-CONTENTS of VOLUME.

If opening this kind of volume results in opening its VOLUME-CONTENTS too,
also return as a second value a list of fresh OPENED-VOLUME values
corresponding to the VOLUME-CONTENTS of VOLUME.  In this case, the caller
should not attempt to call OPEN-VOLUME on the VOLUME-CONTENTS of VOLUME."))

(defgeneric close-volume (volume)
  (:documentation
   "Inverse of OPEN-VOLUME: `kpartx -d`, `cryptsetup luksClose`, etc.
Return values, if any, should be ignored."))

(defmethod close-volume ((volume volume))
  "Default implementation: assume there is nothing to close."
  (values))

(defclass physical-disk (top-level-volume opened-volume) ()
  (:documentation
   "A physical disk drive attached to the machine, which always has a
corresponding block device in /dev available to access it.  Should be used for
whole disks, not partitions (e.g. /dev/sda, not /dev/sda1)."))


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

(defclass-opened-volume opened-raw-disk-image (raw-disk-image))

;; kpartx(1) can operate directly upon raw disk images, and will also make the
;; whole disk image accessible as a loop device, so we could examine the type
;; of (volume-contents volume), and if we find it's PARTITIONED-VOLUME, we
;; could call (open-volume-contents (volume-contents volume) file) and cons an
;; instance of OPENED-VOLUME for the whole disk onto the front of it (from
;; partitions at /dev/mapper/loopNpM we can infer that /dev/loopN is the whole
;; disk).  But for simplicity and composability, just make the whole disk
;; image accessible at this step of the recursion.
(defmethod open-volume ((volume raw-disk-image) (file null))
  (make-opened-volume
   volume
   (ensure-pathname
    (stripln (run "losetup" "--show" "-f" (image-file volume))))))

(defmethod close-volume ((volume opened-raw-disk-image))
  (mrun "losetup" "-d" (device-file volume)))

(defmethod create-volume ((volume raw-disk-image) (file null))
  "Ensure that a raw disk image exists.  Will overwrite only regular files."
  (let ((file (image-file volume)))
    (when (test "-L" file "-o" "-e" file "-a" "!" "-f" file)
      (failed-change "~A already exists and is not a regular file." file))
    ;; Here, following Propellor, we want to ensure that the disk image size
    ;; is a multiple of 4096 bytes, so that the size is aligned to the common
    ;; sector sizes of both 512 and 4096.  But since we currently only support
    ;; volume sizes in whole mebibytes, we know it's already aligned.
    (file:does-not-exist file)
    (mrun
     "fallocate" "-l" (format nil "~DM" (volume-minimum-size volume)) file)))


;;;; Partitioned block devices and their partitions

;;; No support for MSDOS partition tables.

(defclass partitioned-volume (volume)
  ((volume-contents
    :type cons
    :documentation "A list of partitions."))
  (:documentation "A device with a GPT partition table and partitions."))

(defclass-opened-volume opened-partitioned-volume (partitioned-volume))

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

(defclass-opened-volume opened-partition (partition))

(defmethod volume-contents-minimum-size ((volume partitioned-volume))
  "Add one mebibyte for the GPT metadata."
  (1+ (call-next-method)))

(defmethod open-volume ((volume partitioned-volume) (file pathname))
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
    (values
     (make-opened-volume volume file)
     (loop for partition in (volume-contents volume) and loopdev in loopdevs
           collect (make-opened-volume partition loopdev)))))

(defmethod close-volume ((volume opened-partitioned-volume))
  (mrun "kpartx" "-d" (device-file volume)))

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

(defmethod open-volume ((volume lvm-volume-group) (file null))
  (mrun "vgscan")
  (mrun "vgchange" "-ay" (volume-label volume))
  ;; return (as a second value) a list of OPENED-VOLUME for each logical volume
  )

(defclass lvm-logical-volume (volume)
  ((volume-label
    :documentation "The name of the LV, often starting with \"lv_\".")))

(defclass lvm-physical-volume (volume)
  ((volume-group
    :type string
    :initarg :volume-group
    :accessor volume-group
    :documentation
    "The name of the LVM volume group to which this volume belongs."))
  (:documentation "An LVM physical volume.
We do not specify what logical volumes it contains."))


;;;; Filesystems

(defparameter *mount-below* #P""
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

(defclass-opened-volume mounted-filesystem (filesystem))

(defmethod open-volume ((volume filesystem) (file pathname))
  (let ((mount-point
          (merge-pathnames (enough-pathname (mount-point volume) #P"/")
                           (ensure-directory-pathname *mount-below*))))
    (file:directory-exists mount-point)
    (mrun "mount" file mount-point))
  (make-opened-volume volume file))

(defmethod close-volume ((volume mounted-filesystem))
  (mrun "umount" (device-file volume)))

(defclass ext4-filesystem (filesystem) ())

(defclass-opened-volume
    mounted-ext4-filesystem (ext4-filesystem mounted-filesystem))

(defmethod create-volume ((volume ext4-filesystem) (file pathname))
  (mrun :inform
        "mkfs.ext4" file (and (slot-boundp volume 'volume-label)
                              `("-L" ,(volume-label volume)))))

(defclass fat32-filesystem (filesystem) ())

(defclass-opened-volume
    mounted-fat32-filesystem (fat32-filesystem mounted-filesystem))

(defmethod create-volume ((volume fat32-filesystem) (file pathname))
  (mrun :inform
        "mkdosfs" "-F" "32" (and (slot-boundp volume 'volume-label)
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

(defmethod open-volume ((volume luks-container) (file pathname))
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


;;;; Recursive operations

(defmacro with-open-volumes ((volumes &key mount-below) &body forms)
  "Where each of VOLUMES is a VOLUME which may be opened by calling OPEN-VOLUME
with NIL as the second argument, recursively open each of VOLUMES and any
contents thereof, execute forms, and close all volumes that were opened.
MOUNT-BELOW specifies the pathname to be prefixed to mount points when opening
FILESYSTEM volumes."
  (with-gensyms (opened-volumes)
    (flet ((mount-below (form)
             (if mount-below
                 `(let ((*mount-below* ,mount-below)) ,form)
                 form)))
      `(let (,opened-volumes)
         (unwind-protect
              (progn
                (labels
                    ((open-volume-and-contents (volume file)
                       (multiple-value-bind (opened opened-contents)
                           ,(mount-below '(open-volume volume file))
                         (setq ,opened-volumes
                               (append opened-contents
                                       (cons opened ,opened-volumes)))
                         (dolist (opened-volume
                                  (or opened-contents (list opened)))
                           (when (slot-boundp opened-volume 'volume-contents)
                             (open-volume-and-contents
                              (volume-contents opened-volume)
                              (device-file opened-volume)))))))
                  (mapc (rcurry #'open-volume-and-contents nil) ,volumes))
                ,@forms)
           ,(mount-below `(mapc #'close-volume ,opened-volumes)))))))

(defmethod create-volume-and-contents ((volume volume) file)
  "Recursively create VOLUME and its contents, on or at FILE.
**THIS FUNCTION UNCONDITIONALLY FORMATS DISKS, POTENTIALLY DESTROYING DATA**"
  (let (opened-volumes)
    (labels
        ((create (volume file)
           (create-volume volume file)
           (when (slot-boundp volume 'volume-contents)
             (multiple-value-bind (opened opened-contents)
                 (open-volume volume file)
               (setq opened-volumes
                     (append opened-contents (cons opened opened-volumes)))
               (if opened-contents
                   (dolist (opened-volume opened-contents)
                     (when (slot-boundp opened-volume 'volume-contents)
                       (create (volume-contents opened-volume)
                               (device-file opened-volume))))
                   (create (volume-contents opened) (device-file opened)))))))
      (unwind-protect (create volume file)
        (mapc #'close-volume opened-volumes)))))


;;;; Properties

(defprop has-volumes :posix (&rest volumes)
  "Specify non-removable volumes normally accessible to the kernel on this host."
  (:desc (declare (ignore volumes))
         "Has specified volumes.")
  (:hostattrs
   (os:required 'os:linux)
   (apply #'push-hostattrs :volumes volumes)))
