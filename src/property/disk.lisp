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
    :type string :initarg :volume-label :accessor volume-label
    :documentation "The name or label of the volume.
Can only be recorded in or on the volume itself for certain subclasses.  For
example, mostly meaningless for a Linux swap partition.")
   (volume-contents
    :type volume :initarg :volume-contents :accessor volume-contents)
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
    :initarg :volume-size :accessor volume-size
    :documentation "The size of the volume, in whole mebibytes (MiB).
The special value :REMAINING means all remaining free space in the volume
containing this one.

If a larger size is required to accommodate the VOLUME-CONTENTS of the volume
plus any metadata (e.g. partition tables), this value will be ignored.")
   (volume-bootloaders
    :type list :initarg :boots-with :accessor volume-bootloaders
    :documentation
    "List or list of lists specifying bootloaders to be installed to this volume.
The first element of each list is a symbol identifying the type of bootloader,
and the remaining elements are a plist of keyword arguments to be passed to
the implementation of INSTALLER:INSTALL-BOOTLOADER for that bootloader type.

Typically only the top level PHYSICAL-DISK of a host's volumes will have this
slot bound."))
  (:documentation
   "Something which contains filesystems and/or other volumes."))

(define-simple-print-object volume)

(defgeneric copy-volume-and-contents
    (volume &rest initialisations &key &allow-other-keys)
  (:documentation
   "Make a fresh copy of VOLUME, shallowly, except for the VOLUME-CONTENTS of
volume, which is recursively copied.  Keyword arguments may be used to
subsequently replace the copied values of some slots.")
  (:method ((volume volume) &rest initialisations &key &allow-other-keys)
    (let* ((class (class-of volume))
           (copy (allocate-instance class))
           (contents-bound-p (slot-boundp volume 'volume-contents))
           (contents (and contents-bound-p (volume-contents volume))))
      (dolist (slot-name (delete 'volume-contents
                                 (mapcar #'closer-mop:slot-definition-name
                                         (closer-mop:class-slots class))))
        (when (slot-boundp volume slot-name)
          (setf (slot-value copy slot-name) (slot-value volume slot-name))))
      (when contents-bound-p
        (setf (volume-contents copy)
              (if (listp contents)
                  (mapcar #'copy-volume-and-contents contents)
                  (copy-volume-and-contents contents))))
      (apply #'reinitialize-instance copy initialisations))))

(defgeneric subvolumes-of-type (type volume)
  (:documentation
   "Recursively examine VOLUME and its VOLUME-CONTENTS and return a list of all
volumes encountered whose type is a subtype of TYPE.
Returns as a second value a corresponding list of the immediate parents of
each returned volume.")
  (:method ((type symbol) (volume volume))
    (labels ((walk (volume parent &aux (second-arg (list volume)))
               (multiple-value-bind (contents contents-parents)
                   (and (slot-boundp volume 'volume-contents)
                        (multiple-value-mapcan
                         #'walk (ensure-cons (volume-contents volume))
                         (rplacd second-arg second-arg)))
                 (if (subtypep (type-of volume) type)
                     (values (cons volume contents)
                             (cons parent contents-parents))
                     (values contents contents-parents)))))
      (walk volume nil))))

(defgeneric all-subvolumes (volume)
  (:documentation
   "Recursively examine VOLUME and its VOLUME-CONTENTS and return a list of all
volumes encountered.")
  (:method ((volume volume))
    (subvolumes-of-type 'volume volume)))

(defgeneric volume-contents-minimum-size (volume)
  (:documentation
   "Return the minimum size required to accommodate the VOLUME-CONTENTS of VOLUME.")
  (:method ((volume volume))
    (if (slot-boundp volume 'volume-contents)
        (reduce #'+ (mapcar #'volume-minimum-size
                            (ensure-cons (volume-contents volume))))
        0)))

(defgeneric volume-minimum-size (volume)
  (:documentation
   "Return the VOLUME-SIZE of the volume or the minimum size required to
accommodate its contents, whichever is larger.")
  (:method ((volume volume))
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
           (volume-contents-minimum-size volume)))))

(defclass top-level-volume (volume) ()
  (:documentation
   "A volume which never appears as the VOLUME-CONTENTS of another volume."))

(defgeneric create-volume (volume file)
  (:documentation
   "Create VOLUME.  FILE is a pathname at or on which to create VOLUME, for types
of VOLUME where that makes sense, and explicitly nil otherwise.
Return values, if any, should be ignored."))

(defgeneric volume-required-data (volume)
  (:documentation
   "Return (IDEN1 . IDEN2) pairs for each item of prerequisite data opening
and/or creating the volume requires.")
  (:method ((volume volume))
    "Default implementation: nothing required."
    nil))

(defun require-volumes-data (volumes)
  "Call REQUIRE-DATA on each item of prerequisite data requires for opening
and/or creating each of VOLUMES.

Called by property :HOSTATTRS subroutines."
  (dolist (pair (mapcan #'volume-required-data
                        (mapcan #'all-subvolumes volumes)))
    (require-data (car pair) (cdr pair))))


;;;; Opened volumes

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
Return values, if any, should be ignored.")
  (:method ((volume volume))
    "Default implementation: assume there is nothing to close."
    (values)))

(defclass opened-volume (volume)
  ((device-file
    :type pathname
    :initarg :device-file
    :accessor device-file
    :documentation "File under /dev giving access to the opened volume."))
  (:documentation
   "A VOLUME object which has been made directly accessible as a block device."))

(defmethod open-volume ((volume opened-volume) file)
  (copy-volume-and-contents volume))

(defgeneric make-opened-volume (volume device-file)
  (:documentation
   "Where there is a class which is a subclass of both the class of VOLUME and
OPENED-VOLUME, make a fresh instance of that class copying all slots from
VOLUME, and setting the DEVICE-FILE slot to DEVICE-FILE."))

(defmacro defclass-opened-volume
    (name (subclass-of-volume &rest other-superclasses)
     &key (device-file-type 'pathname))
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
         ((volume ,subclass-of-volume) (device-file ,device-file-type))
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

(defclass physical-disk (top-level-volume opened-volume) ()
  (:documentation
   "A physical disk drive attached to the machine, which always has a
corresponding block device in /dev available to access it.  Should be used for
whole disks, not partitions (e.g. /dev/sda, not /dev/sda1)."))


;;;; Disk images

(defclass disk-image (volume)
  ((image-file :initarg :image-file :accessor image-file)))


;;;; Raw disk images

(defclass raw-disk-image (disk-image) ()
  (:documentation
   "A raw disk image, customarily given an extension of .img, suitable for
directly writing out with dd(1)."))

(defclass-opened-volume opened-raw-disk-image (raw-disk-image))

;; kpartx(1) can operate directly upon raw disk images, and will also make the
;; whole disk image accessible as a loop device whose name we can infer from
;; the kpartx(1) output (from partitions at /dev/mapper/loopNpM we can infer
;; that /dev/loopN is the whole disk).  So we could examine the type of
;; (volume-contents volume), and if we find it's PARTITIONED-VOLUME, we could
;; skip executing losetup(1), and just call (open-volume (volume-contents
;; volume) file) and convert the return values into something appropriate for
;; us to return.  But for simplicity and composability, just make the whole
;; disk image accessible at this step of the recursion.
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
    (when (remote-test "-L" file "-o" "-e" file "-a" "!" "-f" file)
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
    :type list
    :documentation "A list of partitions."))
  (:documentation "A device with a GPT partition table and partitions."))

(defclass-opened-volume opened-partitioned-volume (partitioned-volume))

(defclass partition (volume)
  ((partition-typecode
    :initform #x8300 :initarg :partition-typecode :accessor partition-typecode
    :documentation
    "The type code for the partition; see the --typecode option to sgdisk(1).
Either a two-byte hexadecimal number, or a string specifying the GUID.

On GNU/Linux systems, you typically only need to set this to a non-default
value in the case of EFI system partitions, for which case use #xEF00.")
   (partition-bootable
    :initform nil :initarg :partition-bootable :accessor partition-bootable
    :documentation "Whether the legacy BIOS bootable attribute is set.")
   (partition-start-sector
    :type integer
    :initform 0
    :initarg :partition-start-sector
    :accessor partition-start-sector
    :documentation "The sector at which the partition should start.
The default value of 0 means the next free sector.")
   (partition-sectors
    :type integer
    :initarg :partition-sectors
    :accessor partition-sectors
    :documentation "The size of the partition in sectors."))
  (:documentation "A GPT partition."))

(defclass-opened-volume opened-partition (partition))

(defmethod volume-contents-minimum-size ((volume partitioned-volume))
  "Add two mebibytes for the GPT metadata."
  (+ 2 (call-next-method)))

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
  (mrun "kpartx" "-dv" (device-file volume)))

(defmethod create-volume ((volume partitioned-volume) (file pathname))
  (with-slots (volume-contents) volume
    (mrun :inform "sgdisk" "--zap-all" file)
    (mrun :inform "sgdisk"
          ;; Turn off partition alignment when specific start sectors have
          ;; been specified, so that we can be sure they will be respected.
          ;;
          ;; Given this approach, if specify start sector for one partition,
          ;; probably want to specify for all partitions to ensure alignment.
          (and (loop for partition in volume-contents
                       thereis (plusp (partition-start-sector partition)))
               "--set-alignment=1")
          (loop for partition in volume-contents
                for code = (partition-typecode partition)
                when (and (slot-boundp partition 'volume-size)
                          (slot-boundp partition 'partition-sectors))
                  do (failed-change
                      "~A has both VOLUME-SIZE and PARTITION-SECTORS bound."
                      partition)
                collect (strcat
                         "--new=0:"
                         (format nil "~D" (partition-start-sector partition))
                         ":"
                         (cond
                           ((slot-boundp partition 'partition-sectors)
                            (format nil "~D"
                                    (+ (partition-start-sector partition)
                                       (1- (partition-sectors partition)))))
                           ((and (slot-boundp partition 'volume-size)
                                 (eql (volume-size partition) :remaining))
                            "0")
                           (t
                            (format nil "+~DM"
                                    (volume-minimum-size partition)))))
                collect (strcat "--typecode=0:"
                                (etypecase code
                                  (string code)
                                  (integer (format nil "~X" code))))
                when (partition-bootable partition)
                  collect "--attributes=0:set:2")
          file)))


;;;; LVM

(defclass lvm-physical-volume (volume)
  ((lvm-volume-group
    :type string
    :initarg :volume-group
    :initform
    (simple-program-error "LVM physical volume must have volume group.")
    :accessor lvm-volume-group
    :documentation
    "The name of the LVM volume group to which this volume belongs.")

   ;; pvcreate(8) options
   (data-alignment
    :type string
    :initarg :data-alignment
    :accessor data-alignment
    :documentation "Value for the --dataalignment argument to pvcreate(8).")

   ;; vgcreate(8) options
   (physical-extent-size
    :type string
    :initarg :physical-extent-size
    :accessor physical-extent-size
    :documentation "Value for the --dataalignment argument to vgcreate(8).
Should be the same for all PVs in this VG.")
   (alloc
    :type string
    :initarg :alloc
    :accessor alloc
    :documentation "Value for the --alloc argument to vgcreate(8).
Should be the same for all PVs in this VG."))

  (:documentation "An LVM physical volume.
We do not specify what logical volumes it contains."))

(defclass-opened-volume opened-lvm-physical-volume (lvm-physical-volume))

(defmethod open-volume ((volume lvm-physical-volume) (file pathname))
  (make-opened-volume volume file))

(defmethod create-volume ((volume lvm-physical-volume) (file pathname))
  (mrun :inform "pvcreate"
        (and (slot-boundp volume 'data-alignment)
             `("--dataalignment" ,(data-alignment volume)))
        file)
  (if (memstr= (lvm-volume-group volume) (all-lvm-volume-groups))
      (mrun :inform "vgextend" (lvm-volume-group volume) file)
      (mrun :inform "vgcreate" "--systemid" ""
            (and (slot-boundp volume 'physical-extent-size)
                 `("--physicalextentsize" ,(physical-extent-size volume)))
            (and (slot-boundp volume 'alloc)
                 `("--alloc" ,(alloc volume)))
            (lvm-volume-group volume) file)))

(defun all-lvm-volume-groups ()
  (mapcar (curry #'string-trim " ")
          (runlines "vgs" "--no-headings" "-ovg_name")))

(defclass lvm-logical-volume (top-level-volume)
  ((volume-label
    :initform (simple-program-error "LVs must have names.")
    :documentation "The name of the LV, often starting with \"lv_\".")
   (lvm-volume-group
    :type string
    :initarg :volume-group
    :initform
    (simple-program-error "LVM logical volumes must have a volume group.")
    :accessor lvm-volume-group
    :documentation
    "The name of the LVM volume group to which this volume belongs.")))

(defclass-opened-volume activated-lvm-logical-volume (lvm-logical-volume))

(defmethod volume-contents-minimum-size ((volume lvm-logical-volume))
  "LVs cannot be of zero size."
  (max 1 (call-next-method)))

(defmethod open-volume ((volume lvm-logical-volume) (file null))
  (with-slots (volume-label lvm-volume-group) volume
    (mrun "lvchange" "-ay" (strcat lvm-volume-group "/" volume-label))
    ;; lvm(8) says not to use the /dev/mapper names, but rather /dev/vg/lv
    (make-opened-volume
     volume (merge-pathnames volume-label
                             (ensure-directory-pathname
                              (merge-pathnames lvm-volume-group #P"/dev/"))))))

(defmethod close-volume ((volume activated-lvm-logical-volume))
  (mrun "lvchange" "-an"
        (strcat (lvm-volume-group volume) "/" (volume-label volume))))

(defmethod create-volume ((volume lvm-logical-volume) (file null))
  (with-slots (volume-label lvm-volume-group) volume
    ;; Check that the VG exists.
    (unless (memstr= lvm-volume-group (all-lvm-volume-groups))
      (failed-change "Looks like no PVs for VG ~A?" lvm-volume-group))
    ;; Create the LV.
    (mrun :inform "lvcreate" "-Wn"
          (if (and (slot-boundp volume 'volume-size)
                   (eql (volume-size volume) :remaining))
              '("-l" "100%FREE")
              `("-L" ,(format nil "~DM" (volume-minimum-size volume))))
          lvm-volume-group "-n" volume-label)))

(defprop host-lvm-logical-volumes-exist :lisp ()
  (:desc "Host LVM logical volumes all exist")
  (:hostattrs (os:required 'os:linux))
  (:apply
   (assert-remote-euid-root)
   (let* ((existing-lvs
            (loop for (lv vg) in (mapcar #'words (cdr (runlines "lvs")))
                  collect (cons lv vg)))
          (to-create
            ;; LVs are TOP-LEVEL-VOLUMEs.
            (loop for volume in (get-hostattrs :volumes)
                  when (subtypep (class-of volume) 'lvm-logical-volume)
                    unless (member (cons (volume-label volume)
                                         (lvm-volume-group volume))
                                   existing-lvs :test #'equal)
                      collect volume))
          (to-mount
            (loop for volume in to-create
                  nconc (loop for volume
                                in (subvolumes-of-type 'filesystem volume)
                              when (slot-boundp volume 'mount-point)
                                collect volume))))
     (if to-create
         (prog2
             (ignoring-hostattrs
              (consfigurator.property.fstab:has-entries-for-volumes to-create))
             (create-volumes-and-contents to-create)
           (dolist (volume to-create)
             (open-volume volume nil))
           (dolist (volume to-mount)
             (consfigurator.property.mount:mounted
              :target (mount-point volume))))
         :no-change))))


;;;; Filesystems

(defparameter *mount-below* #P"/"
  "Prefix for all filesystem mount points.  Bound by functions to request that
filesystems be mounted relative to a different filesystem root, e.g. under a
chroot.")

(defclass filesystem (volume)
  ((mount-point :type pathname :initarg :mount-point :accessor mount-point)
   (mount-options
    :type list :initform nil :initarg :mount-options :accessor mount-options)
   (extra-space
    :type integer :initform 0 :initarg :extra-space :accessor extra-space
    :documentation
    "When creating the filesystem to accommodate a directory tree whose size is
already known, add this many whole mebibytes of extra free space where
possible.  Ignored if VOLUME-SIZE is also bound."))
  (:documentation
   "A block device containing a filesystem, which can be mounted."))

(defclass-opened-volume mounted-filesystem (filesystem))

(defmethod open-volume ((volume filesystem) (file pathname))
  (let* ((mount-point (chroot-pathname (mount-point volume) *mount-below*))
         (opened-volume (make-opened-volume volume file)))
    (setf (mount-point opened-volume) mount-point)
    (file:directory-exists mount-point)
    (mrun "mount" file mount-point)
    opened-volume))

(defmethod close-volume ((volume mounted-filesystem))
  (mrun "umount" (device-file volume)))

(defclass ext4-filesystem (filesystem)
  ((mount-options :initform '("relatime"))))

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
  ((luks-passphrase-iden1
    :type string :initform "--luks-passphrase" :initarg :luks-passphrase-iden1)
   (luks-type
    :type string :initform "luks" :initarg :luks-type :accessor luks-type
    :documentation
    "The value of the --type parameter to cryptsetup luksFormat.
Note that GRUB2 older than 2.06 cannot open the default LUKS2 format, so
specify \"luks1\" if this is needed.")
   (cryptsetup-options
    :type list :initform nil :initarg :cryptsetup-options
    :documentation
    "Extra arguments to pass to cryptsetup(8) when creating the volume, such as
'--cipher'.  Use the LUKS-TYPE slot for '--type'.")
   (crypttab-options
    :type list :initform '("luks" "discard" "initramfs")
    :initarg :crypttab-options :accessor crypttab-options)
   (crypttab-keyfile :initarg :crypttab-keyfile :accessor crypttab-keyfile)))

(defclass-opened-volume opened-luks-container (luks-container))

(defmethod volume-required-data ((volume luks-container))
  (with-slots (luks-passphrase-iden1 volume-label) volume
    (list (cons luks-passphrase-iden1 volume-label))))

(defmethod open-volume ((volume luks-container) (file pathname))
  (with-slots (luks-passphrase-iden1 volume-label) volume
    (unless (and (stringp volume-label) (plusp (length volume-label)))
      (failed-change "LUKS volume has invalid VOLUME-LABEL."))
    (mrun "cryptsetup" "-d" "-" "luksOpen" file volume-label
          :input (get-data-string luks-passphrase-iden1 volume-label))
    (make-opened-volume volume
                        (merge-pathnames volume-label #P"/dev/mapper/"))))

(defmethod create-volume ((volume luks-container) (file pathname))
  (with-slots
        (luks-passphrase-iden1 volume-label luks-type cryptsetup-options)
      volume
    (mrun :inform
          :input (get-data-string luks-passphrase-iden1 (volume-label volume))
          "cryptsetup" "--type" luks-type
          (and (member luks-type '("luks" "luks2") :test #'string=)
               `("--label" ,volume-label))
          cryptsetup-options
          "luksFormat" file "-")))

(defmethod close-volume ((volume opened-luks-container))
  (mrun "cryptsetup" "luksClose" (device-file volume)))

(defclass linux-swap (volume) ())

(defmethod create-volume ((volume linux-swap) (file pathname))
  (mrun "mkswap" file))


;;;; Recursive operations

(defmacro with-mount-below (form)
  "Avoid establishing any binding for *MOUNT-BELOW* when the caller did not
explicitly request one."
  `(if mount-below-supplied-p
       (let ((*mount-below* mount-below)) ,form)
       ,form))

(defun open-volumes-and-contents
    (volumes &key (mount-below nil mount-below-supplied-p))
  "Where each of VOLUMES is a VOLUME which may be opened by calling OPEN-VOLUME
with NIL as the second argument, recursively open each of VOLUMES and any
contents thereof, and return a list of the volumes that were opened, in the
order in which they should be closed, and as a second value, a corresponding
list of the immediate parents of each opened volume.  MOUNT-BELOW specifies a
pathname to prefix to mount points when opening FILESYSTEM volumes.

Also return as third and fourth values a list of volumes encountered that were
already open and a corresponding list of their immediate parents.

Calling this function can be useful for testing at the REPL, but code should
normally use WITH-OPEN-VOLUMES or WITH-OPENED-VOLUMES.

If an error is signalled while the attempt to open volumes is in progress, a
single attempt will be made to close all volumes opened up to that point."
  (let
      (opened-volumes opened-parents already-open already-parents filesystems)
    (handler-case
        (labels
            ((open-volume-and-contents (volume file parent)
               ;; Postpone filesystems until the end so that we can sort
               ;; them before mounting, to avoid unintended shadowing.
               (if (subtypep (type-of volume) 'filesystem)
                   (push (list parent volume file) filesystems)
                   (multiple-value-bind (opened opened-contents)
                       (open-volume volume file)
                     (let ((opened-contents-parents
                             (make-list (length opened-contents)
                                        :initial-element volume)))
                       ;; Don't queue for closure volumes which were already
                       ;; open before we began.
                       (if (subtypep (class-of volume) 'opened-volume)
                           (setq opened-volumes
                                 (append opened-contents opened-volumes)
                                 opened-parents
                                 (nconc opened-contents-parents
                                        opened-parents)
                                 already-open
                                 (cons volume already-open)
                                 already-parents
                                 (cons parent already-parents))
                           (setq opened-volumes
                                 (append opened-contents
                                         (cons opened opened-volumes))
                                 opened-parents
                                 (nconc opened-contents-parents
                                        (cons parent opened-parents)))))
                     (dolist (opened-volume (or opened-contents `(,opened)))
                       (when (slot-boundp opened-volume 'volume-contents)
                         (open-volume-and-contents
                          (volume-contents opened-volume)
                          (device-file opened-volume)
                          opened-volume)))))))
          (mapc (rcurry #'open-volume-and-contents nil nil) volumes)
          ;; Note that filesystems never have any VOLUME-CONTENTS to open.
          (with-mount-below
              (dolist (filesystem
                       (nreverse
                        (sort filesystems #'subpathp
                              :key (compose #'ensure-directory-pathname
                                            #'mount-point
                                            #'cadr))))
                (push (pop filesystem) opened-parents)
                (push (apply #'open-volume filesystem) opened-volumes)))
          (values opened-volumes opened-parents already-open already-parents))
      (serious-condition (condition)
        (unwind-protect (mapc #'close-volume opened-volumes)
          (error condition))))))

(defmacro with-open-volumes ((volumes
                              &key
                                (mount-below nil mount-below-supplied-p)
                                opened-volumes)
                             &body forms)
  "Where each of VOLUMES is a VOLUME which may be opened by calling OPEN-VOLUME
with NIL as the second argument, recursively open each of VOLUMES and any
contents thereof, execute forms, and close all volumes that were opened.

MOUNT-BELOW specifies a pathname to prefix to mount points when opening
FILESYSTEM volumes.  OPENED-VOLUMES specifies a symbol to which a list of all
volumes that were opened will be bound, which can be used to do things like
populate /etc/fstab and /etc/crypttab.  Do not modify this list."
  (let ((opened-volumes (or opened-volumes (gensym))))
    `(let ((,opened-volumes (open-volumes-and-contents
                             ,volumes
                             ,@(and mount-below-supplied-p
                                    `(:mount-below ,mount-below)))))
       (unwind-protect (progn ,@forms)
         (mrun "sync")
         (mapc #'close-volume ,opened-volumes)))))

(defmacro with-opened-volumes
    ((volumes &key (mount-below nil mount-below-supplied-p) leave-open)
     &body propapps)
  "Macro property combinator.  Where each of VOLUMES is a VOLUME which may be
opened by calling OPEN-VOLUME with NIL as the second argument, recursively
open each of VOLUMES and any contents thereof, apply PROPAPPS, and, unless
LEAVE-OPEN, close all volumes that were opened.

MOUNT-BELOW specifies a pathname to prefix to mount points when opening
FILESYSTEM volumes.  During the application of PROPAPPS, all
'DISK:OPENED-VOLUMES and 'DISK:OPENED-VOLUME-PARENTS connattrs are replaced
with lists of the volumes that were opened/already open and corresponding
immediate parent volumes."
  `(with-opened-volumes*
     ',volumes
     ,(if (cdr propapps) `(eseqprops ,@propapps) (car propapps))
     ,@(and mount-below-supplied-p `(:mount-below ,mount-below))
     :leave-open ,leave-open))

(define-function-property-combinator with-opened-volumes*
    (volumes propapp &key (mount-below nil mount-below-supplied-p) leave-open)
  (:retprop
   :type (propapp-type propapp)
   :hostattrs (lambda-ignoring-args
                (require-volumes-data volumes)
                (propapp-attrs propapp))
   :apply
   (lambda-ignoring-args
     (multiple-value-bind
           (opened-volumes opened-parents already-open already-parents)
         (apply #'open-volumes-and-contents
                `(,volumes ,@(and mount-below-supplied-p
                                  `(:mount-below ,mount-below))))
       (with-connattrs
           ('opened-volumes (append opened-volumes already-open)
            'opened-volume-parents (append opened-parents already-parents))
         (unwind-protect (apply-propapp propapp)
           (mrun "sync")
           (unless leave-open (mapc #'close-volume opened-volumes))))))
   :args (cdr propapp)))

(defun create-volumes-and-contents (volumes &optional files)
  "Where each of VOLUMES is a VOLUME which may be created by calling
CREATE-VOLUME with the corresponding entry of FILES, or NIL, as a second
argument, recursively create each of VOLUMES and any contents thereof.
**THIS FUNCTION UNCONDITIONALLY FORMATS DISKS, POTENTIALLY DESTROYING DATA.**"
  (let (opened-volumes)
    (labels
        ((create-volume-and-contents
             (volume file
              &aux
                (already-opened (subtypep (class-of volume) 'opened-volume)))
           (unless already-opened
             (create-volume volume file))
           (when (slot-boundp volume 'volume-contents)
             (multiple-value-bind (opened opened-contents)
                 (open-volume volume file)
               (setq opened-volumes
                     (append opened-contents
                             ;; Don't queue for closure volumes which were
                             ;; already open before we began.
                             (if already-opened
                                 opened-volumes
                                 (cons opened opened-volumes))))
               (if opened-contents
                   (dolist (opened-volume opened-contents)
                     (when (slot-boundp opened-volume 'volume-contents)
                       (create-volume-and-contents
                        (volume-contents opened-volume)
                        (device-file opened-volume))))
                   (create-volume-and-contents
                    (volume-contents opened) (device-file opened)))))))
      (unwind-protect
           (mapc #'create-volume-and-contents
                 volumes (loop repeat (length volumes) collect (pop files)))
        (mrun "sync")
        (mapc #'close-volume opened-volumes)))))


;;;; Properties

(defmacro has-volumes (&rest volume-specifications)
  "Specify non-removable volumes normally accessible to the kernel on this host.

The order of the list of volumes is significant: it is the order in which
attempts to open all of the volumes should be made.  So, for example, any LVM
volume groups should occur later in the list than the partitions containing
the LVM physical volumes corresponding to those volume groups."
  `(has-volumes* (volumes ,@volume-specifications)))

(defprop has-volumes* :posix (volumes)
  (:desc "Has specified volumes.")
  (:hostattrs
   (os:required 'os:linux)
   (push-hostattrs :volumes volumes)))

;; TODO This should probably be in another package, and exported from there.
(defproplist caches-cleaned :posix ()
  "Clean all caches we know how to clean in preparation for image creation."
  (:desc "Caches cleaned")
  (file:data-cache-purged)
  (os:typecase
    (debianlike (apt:cache-cleaned))))

(defprop %raw-image-created :lisp (volumes &key chroot rebuild)
  (:desc (declare (ignore volumes chroot rebuild))
         #?"Created raw disk image & other volumes")
  (:hostattrs
   (require-volumes-data volumes)
   ;; We require GNU du(1).
   (os:required 'os:linux))
  (:check
   (declare (ignore chroot))
   (and
    (not rebuild)
    (file-exists-p
     (image-file
      (find-if (rcurry #'subtypep 'raw-disk-image) volumes :key #'type-of)))))
  (:apply
   (declare (ignore rebuild))
   (multiple-value-bind (mount-points volumes)
       ;; Find all mount points, and make modifiable copies of volumes
       ;; containing filesystems without VOLUME-SIZE, which we'll set.
       (loop for volume in volumes
             for filesystems
               = (delete-if-not (rcurry #'slot-boundp 'mount-point)
                                (subvolumes-of-type 'filesystem volume))
             nconc (mapcar #'mount-point filesystems) into mount-points
             if (loop for filesystem in filesystems
                        thereis (not (slot-boundp filesystem 'volume-size)))
               collect (copy-volume-and-contents volume) into volumes
             else collect volume into volumes
             finally (return (values mount-points volumes)))
     ;; Do the VOLUME-SIZE updates.  For now we make the assumption that a
     ;; copy of the files made by rsync will fit in a disk of 1.1 times the
     ;; size of however much space the files are taking up on whatever
     ;; filesystem underlies the chroot.  An alternative would be to find the
     ;; actual size of each file's data and round it up to the block size of
     ;; FILESYSTEM, which could be stored in a slot.  Since some filesystems
     ;; are able to store more than one file per block, we would probably want
     ;; a method on filesystem types to compute the expected size the file
     ;; will take up, call that on each file, and sum.
     (dolist (filesystem
              (mapcan (curry #'subvolumes-of-type 'filesystem) volumes))
       (when (and (slot-boundp filesystem 'mount-point)
                  (not (slot-boundp filesystem 'volume-size)))
         (let ((dir (mount-point filesystem)))
           (setf (volume-size filesystem)
                 (+ (ceiling
                     (* 1.1
                        (parse-integer
                         (car
                          (split-string
                           (run "du" "-msx" (chroot-pathname dir chroot)
                                (loop for mount-point in mount-points
                                      unless (eql mount-point dir)
                                        collect (strcat "--exclude="
                                                        (unix-namestring
                                                         (chroot-pathname
                                                          mount-point chroot))
                                                        "/*"))))))))
                    (extra-space filesystem))))))
     ;; Finally, create the volumes.
     (create-volumes-and-contents volumes))))

(defun image-chroot (image-pathname)
  (ensure-directory-pathname
   (strcat (unix-namestring image-pathname) ".chroot")))

(defun host-volumes-just-one-physical-disk (host fun)
  (loop
    with found
    for volume in (get-hostattrs :volumes (preprocess-host host))
    for physical-disk-p = (subtypep (type-of volume) 'physical-disk)
    if (and physical-disk-p (not found) (slot-boundp volume 'volume-contents))
      do (setq found t)
      and collect (aprog1 (copy-volume-and-contents volume) (funcall fun it))
    else unless physical-disk-p
           collect volume
    finally
       (unless found
         (inapplicable-property
          "Volumes list for host has no DISK:PHYSICAL-DISK with contents."))))

(defpropspec raw-image-built-for :lisp
    (options host image-pathname &key rebuild)
  "Build a raw disk image for HOST at IMAGE-PATHNAME.
The image corresponds to the first DISK:PHYSICAL-DISK entry in the host's
volumes, as specified using DISK:HAS-VOLUMES; there must be at least one such
entry.  Other DISK:PHYSICAL-DISK entries will be ignored, so ensure that none
of the properties of the host will write to areas of the filesystem where
filesystems stored on other physical disks would normally be mounted.

OPTIONS will be passed on to CHROOT:OS-BOOTSTRAPPED-FOR, which see.

In most cases you will need to ensure that HOST has properties which do at
least the following:

  - declare the host's OS

  - install a kernel

  - install the binaries/packages needed to install the host's bootloader to
    its volumes (usually with INSTALLER:BOOTLOADER-BINARIES-INSTALLED).

Unless REBUILD, the image will not be repartitioned even if the specification
of the host's volumes changes, although the contents of the image's
filesystems will be incrementally updated when other properties change."
  (:desc #?"Built image for ${(get-hostname host)} @ ${image-pathname}")
  (let ((chroot (image-chroot image-pathname))
        (volumes (host-volumes-just-one-physical-disk
                  host (lambda (volume)
                         (change-class volume 'raw-disk-image)
                         (setf (image-file volume) image-pathname)))))
    `(on-change (eseqprops
                 ,(propapp (chroot:os-bootstrapped-for. options chroot host
                             (caches-cleaned)))
                 (%raw-image-created
                  ,volumes :chroot ,chroot :rebuild ,rebuild))
       (consfigurator.property.installer:files-installed-to-volumes-for
        nil ,host ,volumes :chroot ,chroot))))

(defprop %volumes-created :lisp (volumes)
  (:desc "Created host volumes")
  (:hostattrs (os:required 'os:linux) (require-volumes-data volumes))
  (:apply
   (dolist (volume volumes)
     (when (subtypep (type-of volume) 'physical-disk)
       (setf (volume-size volume)
             (floor (/ (parse-integer
                        (stripln
                         (run "blockdev" "--getsize64" (device-file volume))))
                       1048576)))))
   (create-volumes-and-contents volumes)))

(defpropspec first-disk-installed-for :lisp
    (options host device-file &key chroot)
  "Install HOST to the DISK:PHYSICAL-DISK accessible at DEVICE-FILE.
**THIS PROPERTY UNCONDITIONALLY FORMATS DISKS, POTENTIALLY DESTROYING DATA,
  EACH TIME IT IS APPLIED.**

Do not apply in DEFHOST.  Apply with DEPLOY-THESE/HOSTDEPLOY-THESE.

The DISK:VOLUME-CONTENTS of the first DISK:PHYSICAL-DISK entry in the host's
volumes, as specified using DISK:HAS-VOLUMES, will be created at DEVICE-FILE;
there must be at least one such DISK:PHYSICAL-DISK entry.  Other
DISK:PHYSICAL-DISK entries will be ignored, so ensure that none of the
properties of the host will write to areas of the filesystem where filesystems
stored on other physical disks would normally be mounted.

OPTIONS will be passed on to CHROOT:OS-BOOTSTRAPPED-FOR, which see.

In most cases you will need to ensure that HOST has properties which do at
least the following:

  - declare the host's OS

  - install a kernel

  - install the binaries/packages needed to install the host's bootloader to
    its volumes (usually with INSTALLER:BOOTLOADER-BINARIES-INSTALLED).

If CHROOT, an intermediate chroot is bootstrapped at CHROOT, and HOST's
properties are applied to that.  Otherwise, HOST's OS is bootstrapped directly
to DEVICE-FILE.  It's useful to supply CHROOT when you expect to install the
same HOST to a number of physical disks.

Applying this property is similar to applying DISK:RAW-IMAGE-BUILT-FOR and
then immediately dd'ing out the image to DEVICE-FILE.  The advantage of this
property is that there is no need to resize filesystems to fill the size of
the host's actual physical disk upon first boot."
  (:desc #?"Installed ${(get-hostname host)} to ${device-file}")
  (let ((volumes (host-volumes-just-one-physical-disk
                  host (lambda (v)
                         (setf (device-file v)
                               (parse-unix-namestring device-file))))))
    `(eseqprops
      (%volumes-created ,volumes)
      ,@(and chroot
             (list (propapp (chroot:os-bootstrapped-for. options chroot host
                              (caches-cleaned)))))
      (consfigurator.property.installer:files-installed-to-volumes-for
       ,options ,host ,volumes :chroot ,chroot))))

(defpropspec volumes-installed-for :lisp (options host &key chroot leave-open)
  "Install HOST to its volumes, as specified using DISK:HAS-VOLUMES.
**THIS PROPERTY UNCONDITIONALLY FORMATS DISKS, POTENTIALLY DESTROYING DATA,
  EACH TIME IT IS APPLIED.**

Do not apply in DEFHOST.  Apply with DEPLOY-THESE/HOSTDEPLOY-THESE.

OPTIONS will be passed on to CHROOT:OS-BOOTSTRAPPED-FOR, which see.

In most cases you will need to ensure that HOST has properties which do at
least the following:

  - declare the host's OS

  - install a kernel

  - install the binaries/packages needed to install the host's bootloader to
    its volumes (usually with INSTALLER:BOOTLOADER-BINARIES-INSTALLED).

If CHROOT, an intermediate chroot is bootstrapped at CHROOT, and HOST's
properties are applied to that.  Otherwise, HOST's OS is bootstrapped directly
to its volumes.  This parameter is useful for the case of installing HOST from
a live system which might not have network access.  See \"Tutorial: OS
installation\" in the Consfigurator user's manual.

If LEAVE-OPEN, HOST's volumes will not be closed.  This allows you to inspect
the result of the installation.  If you want to run this property again, you
should first manually close all the volumes."
  (:desc #?"Installed ${(get-hostname host)}")
  (let ((volumes (get-hostattrs :volumes host)))
    `(eseqprops
      (%volumes-created ,volumes)
      ,@(and chroot
             (list (propapp (chroot:os-bootstrapped-for. options chroot host
                              (caches-cleaned)))))
      (consfigurator.property.installer:files-installed-to-volumes-for
       ,options ,host ,volumes :chroot ,chroot :leave-open ,leave-open))))

(defprop %squashfsed :posix (chroot image &optional (compression "xz"))
  (:apply
   (file:does-not-exist image)
   (with-remote-temporary-file (excludes)
     (write-remote-file
      excludes (format nil "~@{~&~A~}" "/boot" "/proc" "/dev" "/sys" "/run"))
     (run :inform "nice" "mksquashfs" chroot image
          "-no-progress" "-comp" compression "-ef" excludes))))

;; Based on live-wrapper, and some help from this guide:
;;     <https://willhaley.com/blog/custom-debian-live-environment/>
(defpropspec debian-live-iso-built :lisp (options image-pathname properties)
  "Build a Debian Live hybrid ISO at IMAGE-PATHNAME for a host with properties
PROPERTIES, which should specify, at a minimum, the operating system for the
live system.  OPTIONS is a plist of keyword parameters:

  - :CHROOT-OPTIONS -- passed on to CHROOT:OS-BOOTSTRAPPED-FOR, which see.

Currently only BIOS boot is implemented."
  (:desc #?"Debian Live ISO built @ ${image-pathname}")
  (destructuring-bind
      (&key chroot-options
       &aux (chroot (image-chroot image-pathname))
         (iso-root (ensure-directory-pathname
                    (strcat (unix-namestring image-pathname) ".cd")))
         (isolinux (merge-pathnames "isolinux/" iso-root))
         (squashfs (merge-pathnames "live/filesystem.squashfs" iso-root))
         (host (make-host
                :hostattrs '(:hostname ("debian"))
                :propspec
                (append-propspecs
                 properties
                 (make-propspec
                  :propspec
                  '(eseqprops
                    (apt:installed "initramfs-tools" "linux-image-amd64"
                     "live-boot" "task-laptop" "libnss-myhostname"
                     "syslinux-common" "isolinux")
                    (caches-cleaned))))))
         (host-arch (os:debian-architecture (get-hostattrs-car :os host))))
      options
    (unless (member host-arch '(:amd64))
      (inapplicable-property
       "Architecture ~A of live host not supported." host-arch))
    `(eseqprops
      (apt:installed "squashfs-tools" "xorriso")
      (file:directory-exists ,isolinux)
      (file:containing-directory-exists ,squashfs)
      (on-change (chroot:os-bootstrapped-for ,chroot-options ,chroot ,host)

        (%squashfsed ,chroot ,squashfs)

        ;; Copy the chroot's versions of bootloader binaries.
        (file:is-copy-of ,(merge-pathnames "isolinux.bin" isolinux)
                         ,(chroot-pathname "/usr/lib/ISOLINUX/isolinux.bin"
                                           chroot))
        ,@(loop for basename in '("ldlinux" "libcom32" "vesamenu" "libutil"
                                  "libutil" "libmenu" "libgpl" "hdt")
                for file = (strcat basename ".c32")
                collect
                `(file:is-copy-of
                  ,(merge-pathnames file isolinux)
                  ,(chroot-pathname
                    (merge-pathnames file "/usr/lib/syslinux/modules/bios/")
                    chroot)))

        ;; Copy the targets of the symlinks in the root of the chroot.
        (file:is-copy-of ,(merge-pathnames "live/vmlinuz" iso-root)
                         ,(merge-pathnames "vmlinuz" chroot))
        (file:is-copy-of ,(merge-pathnames "live/initrd.img" iso-root)
                         ,(merge-pathnames "initrd.img" chroot))

        (file:exists-with-content ,(merge-pathnames "isolinux.cfg" isolinux)
          ("UI vesamenu.c32"
           ""
           "MENU TITLE Live Boot Menu"
           "DEFAULT linux"
           "TIMEOUT 600"
           "MENU RESOLUTION 640 480"
           ""
           "LABEL linux"
           "  MENU LABEL Debian Live [BIOS/ISOLINUX]"
           "  MENU DEFAULT"
           "  KERNEL /live/vmlinuz"
           "  APPEND initrd=/live/initrd.img boot=live"
           ""
           "LABEL linux"
           "  MENU LABEL Debian Live [BIOS/ISOLINUX] (nomodeset)"
           "  MENU DEFAULT"
           "  KERNEL /live/vmlinuz"
           "  APPEND initrd=/live/initrd.img boot=live nomodeset"))

        (cmd:single
         :inform
         "xorriso" "-as" "mkisofs" "-iso-level" "3" "-o" ,image-pathname
         "-full-iso9660-filenames" "-volid" "DEBIAN_LIVE"
         "-isohybrid-mbr" ,(chroot-pathname "/usr/lib/ISOLINUX/isohdpfx.bin"
                                            chroot)

         "-eltorito-boot" "isolinux/isolinux.bin"
         "-no-emul-boot" "-boot-load-size" "4" "-boot-info-table"
         "--eltorito-catalog" "isolinux/isolinux.cat"

         ,iso-root)))))

;; TODO Possibly we want (a version of) this to not fail, but just do nothing,
;; if the relevant volume groups etc. are inactive?
(defproplist host-logical-volumes-exist :lisp ()
  "Create missing logical volumes, like LVM logical volumes and BTRFS
subvolumes, as specified by DISK:HAS-VOLUMES.  Does not delete or overwrite
anything, aside from editing /etc/fstab in some cases.  Intended to make it
easy to add new logical volumes by just editing the volumes specification.

For logical volumes containing instances of FILESYSTEM with a specified
MOUNT-POINT, ensure there's an /etc/fstab entry and try to mount.

Currently only handling of LVM logical volumes is implemented."
  (:desc "Host logical volumes all exist")
  (host-lvm-logical-volumes-exist))


;;;; Utilities

(defun parse-volume-size (volume-size-specification)
  (if (stringp volume-size-specification)
      (multiple-value-bind (match groups)
          (re:scan-to-strings #?/\A([0-9]+)([MGT])?\z/ volume-size-specification)
        (unless match
          (simple-program-error
           "~A is not a valid volume size." volume-size-specification))
        (* (parse-integer (elt groups 0))
           (eswitch ((elt groups 1) :test #'string=)
             ("M" 1)
             ("G" 1024)
             ("T" 1048576))))
      volume-size-specification))

(defmacro volumes (&body volume-specifications)
  "Return a list of instances of VOLUME, one for each element of
VOLUME-SPECIFICATIONS.  Each of VOLUME-SPECIFICATIONS is an (unquoted) list of
the form (TYPE &REST INITARGS).

TYPE is a symbol naming the volume type to be initialised.  If the symbol does
not name a subclass of VOLUME, it will be replaced with a symbol of the same
name in the DISK package; this allows type names to be used unqualified.

INITARGS is an even-length plist, possibly with a final additional element,
which is either another volume specification or an (unquoted) list of volume
specifications.  This becomes the VOLUME-CONTENTS of the VOLUME.

The following keys in INITARGS are handled specially:

    - :VOLUME-SIZE -- may be a string like \"100M\", \"2G\", \"1T\" which will
      be converted into a whole number of mebibytes.  \"M\", \"G\", and \"T\"
      are currently supported.

Example usage:

  (volumes
    (physical-disk
     (partitioned-volume
      ((partition
        :partition-typecode #xef00
        (fat32-filesystem
         :volume-size \"512M\"
         :mount-point #P\"/boot/efi\"))
       (partition
        (luks-container
         (lvm-physical-volume
          :volume-group \"vg_laptop\"))))))
    (lvm-logical-volume
       :volume-group \"vg_laptop\"
       :volume-label \"lv_laptop_root\"
       (ext4-filesystem :mount-point #P\"/\")))"
  (labels
      ((parse (spec)
         (unless (listp spec)
           (simple-program-error "~A is not a list." spec))
         (let* ((contentsp (not (evenp (length (cdr spec)))))
                (initargs
                  (if contentsp (butlast (cdr spec)) (cdr spec)))
                (contents (and contentsp (lastcar (cdr spec)))))
           (when (loop for key on initargs by #'cddr
                         thereis (and (eql (car key) :volume-size)))
             (setf (getf initargs :volume-size)
                   `(parse-volume-size ,(getf initargs :volume-size))))
           (when (and contents (not (listp contents)))
             (simple-program-error "~A is not a list." contents))
           `(make-instance
             ',(let ((class (find-class (car spec) nil)))
                 (if (and class (subtypep (class-name class) 'volume))
                     (car spec)
                     (intern (symbol-name (car spec))
                             (find-package :consfigurator.property.disk))))
             ,@initargs
             ,@(and contentsp
                    `(:volume-contents
                      ,(if (listp (car contents))
                           `(list ,@(mapcar #'parse contents))
                           (parse contents))))))))
    `(list ,@(mapcar #'parse volume-specifications))))
