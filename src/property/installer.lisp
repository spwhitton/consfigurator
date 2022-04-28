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

(in-package :consfigurator.property.installer)
(named-readtables:in-readtable :consfigurator)

;;;; Bootloaders

(defgeneric install-bootloader-propspec
    (bootloader-type volume running-on-target &key)
  (:documentation
   "Return a propspec expression which installs bootloader of type
BOOTLOADER-TYPE to VOLUME.
The propapp yielded by the propspec may be of type :POSIX or of type :LISP.

RUNNING-ON-TARGET indicates whether the host to which we are connected is the
host the bootloader will boot.  For example, it is NIL when building disk
images, and T when installing a host from a live environment.  Bootloader
installation might behave differently when RUNNING-ON-TARGET is NIL, or error
out."))

(defgeneric install-bootloader-binaries-propspec (bootloader-type volume &key)
  (:documentation
   "Return a propspec expression evaluating to a :POSIX propapp which
fetches/installs whatever binaries/packages need to be available to install
BOOTLOADER-TYPE to VOLUME."))

(defun get-propspecs (volumes running-on-target)
  (loop for volume in (mapcan #'all-subvolumes volumes)
        when (slot-boundp volume 'volume-bootloaders)
          nconc (loop with bls = (volume-bootloaders volume)
                      for bootloader in (if (listp (car bls)) bls (list bls))
                      collect (destructuring-bind (type . args) bootloader
                                (apply #'install-bootloader-propspec
                                       type volume running-on-target args)))))

;; At :HOSTATTRS time we don't have the OPENED-VOLUME values required by the
;; :APPLY subroutines which actually install the bootloaders.  So we call
;; GET-PROPSPECS twice: (in CHROOT-INSTALLED-TO-VOLUMES-FOR) at :HOSTATTRS
;; time to generate propspecs for the sake of running :HOSTATTRS subroutines,
;; and then at :APPLY time where we can get at the OPENED-VOLUME values, we
;; ignore the previously generated propspecs and call GET-PROPSPECS again.
;; This approach should work for any sensible VOLUME<->OPENED-VOLUME pairs.
(define-function-property-combinator
    %install-bootloaders (running-on-target &rest propapps)
  (:retprop
   :type :lisp
   :hostattrs (lambda () (mapc #'propapp-attrs propapps))
   :apply
   (lambda ()
     (mapc #'consfigure
           (get-propspecs (get-connattr :opened-volumes) running-on-target))
     (mrun "sync"))))


;;;; Properties

(defprop %update-target-from-chroot :posix (chroot target)
  (:desc #?"Updated ${target} from ${chroot}")
  (:apply
   (assert-remote-euid-root)
   (run "rsync" "-PSavx" "--delete"
        (loop for volume
                in (mapcan (curry #'subvolumes-of-type 'mounted-filesystem)
                           (get-connattr :opened-volumes))
              collect (strcat "--exclude="
                              (unix-namestring (mount-point volume))))
        (strcat (unix-namestring chroot) "/")
        (strcat (unix-namestring target) "/"))))

(defpropspec chroot-installed-to-volumes-for :lisp
    (host chroot volumes &key running-on-target)
  "Where CHROOT contains the root filesystem of HOST and VOLUMES is a list of
volumes, recursively open the volumes and rsync in the contents of CHROOT.
Also update the fstab and crypttab, and try to install bootloader(s)."
  (:desc #?"${chroot} installed to volumes")
  (let ((target
          (ensure-directory-pathname
           (strcat
            (drop-trailing-slash
             (unix-namestring (ensure-directory-pathname chroot)))
            ".target"))))
    `(with-these-open-volumes (,volumes :mount-below ,target)
       (%update-target-from-chroot ,chroot ,target)
       (chroot:deploys-these
        ,target ,host
        ,(make-propspec
          :propspec
          `(eseqprops
            ,(propapp
              (os:etypecase
                  (debianlike
                   (file:lacks-lines
                    "/etc/fstab" "# UNCONFIGURED FSTAB FOR BASE SYSTEM")
                   ;; These will overwrite any custom mount options, etc.,
                   ;; with values from VOLUMES.  Possibly it would be better
                   ;; to use properties which only update the fs-spec/source
                   ;; fields.  However, given that VOLUMES ultimately comes
                   ;; from the volumes the user has declared for the host, it
                   ;; is unlikely there are other properties setting mount
                   ;; options etc. which are in conflict with VOLUMES.
                   (fstab:has-entries-for-opened-volumes)
                   (crypttab:has-entries-for-opened-volumes))))
            (%install-bootloaders
             ,running-on-target
             ,@(get-propspecs (get-hostattrs :volumes) running-on-target))))))))

(defpropspec bootloader-binaries-installed :posix ()
  "Install whatever binaries/packages need to be available to install the host's
bootloaders to its volumes from within that host.  For example, this might
install a package providing /usr/sbin/grub-install, but it won't execute it."
  (:desc "Bootloader binaries installed")
  (loop
    for volume in (mapcan #'all-subvolumes (get-hostattrs :volumes))
    when (slot-boundp volume 'volume-bootloaders)
      nconc (loop with bls = (volume-bootloaders volume)
                  for bootloader in (if (listp (car bls)) bls (list bls))
                  collect (destructuring-bind (type . args) bootloader
                            (apply #'install-bootloader-binaries-propspec
                                   type volume args)))
        into propspecs
    finally
       (setq propspecs (delete-duplicates propspecs :test #'tree-equal))
       (return
         (if (cdr propspecs) (cons 'eseqprops propspecs) (car propspecs)))))

(defpropspec bootloaders-installed :lisp (&key (running-on-target t))
  "Install the host's bootloaders to its volumes.
Intended to be attached to properties like INSTALLER:CLEANLY-INSTALLED-ONCE
using a combinator like ON-CHANGE, or applied manually with DEPLOY-THESE."
  (:desc "Bootloaders installed")
  `(eseqprops
    (bootloader-binaries-installed)
    ,@(get-propspecs (get-hostattrs :volumes) running-on-target)))


;;;; Live replacement of GNU/Linux distributions

;;; This is based on Propellor's OS.cleanInstallOnce property -- very cool!
;;;
;;; We prepare only a base system chroot, and then apply the rest of the
;;; host's properties after the flip, rather than applying all of the host's
;;; properties to the chroot and only then flipping.  This has the advantage
;;; that properties which normally restrict themselves when running in a
;;; chroot will instead apply all of their changes.  There could be failures
;;; due to still running the old OS's kernel and init system, however, which
;;; might be avoided by applying the properties only to the chroot.
;;;
;;; Another option would be a new SERVICES:WITHOUT-STARTING-SERVICES-UNTIL-END
;;; which would disable starting services and push the cleanup forms inside
;;; the definition of SERVICES:WITHOUT-STARTING-SERVICES to *AT-END-FUNCTIONS*
;;; in a closure.  We'd also want %CONSFIGURE to use UNWIND-PROTECT to ensure
;;; that the AT-END functions get run even when there's a nonlocal exit from
;;; %CONSFIGURE's call to APPLY-PROPAPP; perhaps we could pass a second
;;; argument to the AT-END functions indicating whether there was a non-local
;;; transfer of control.  REBOOT:AT-END might only reboot when there was a
;;; normal return from APPLY-PROPAPP, whereas the cleanup forms from
;;; SERVICES:WITHOUT-STARTING-SERVICES would always be evaluated.

(defprop %root-filesystems-flipped :lisp (new-os old-os)
  (:hostattrs (os:required 'os:linux))
  (:apply
   (assert-remote-euid-root)
   (let ((new-os (ensure-directory-pathname new-os))
         (old-os
           (ensure-directories-exist (ensure-directory-pathname old-os)))
         (preserved-directories
           '(;; This can contain sockets, remote Lisp image output, etc.;
             ;; avoid upsetting any of those.
             #P"/tmp/"
             ;; Makes sense to keep /proc until we replace the running init,
             ;; and we want to retain all the systemd virtual filesystems
             ;; under /sys to avoid problems applying other properties.  Both
             ;; are empty directories right after debootstrap, so nothing to
             ;; copy out.
             #P"/proc/" #P"/sys/"
             ;; This we make use of below.
             #P"/old-run/"))
         efi-system-partition-mount-args)
     (flet ((system (&rest args)
              (alet (loop for arg in args
                          if (pathnamep arg)
                            collect (unix-namestring arg)
                          else collect arg)
                (foreign-funcall "system" :string (sh-escape it) :int)))
            (preservedp (pathname)
              (member pathname preserved-directories :test #'pathname-equal)))
       (mount:assert-devtmpfs-udev-/dev)
       (unless (remote-mount-point-p "/run")
         (failed-change "/run is not a mount point; don't know what to do."))

       ;; If there's an EFI system partition, we need to store knowledge of
       ;; how to mount it so that we can restore the mount after doing the
       ;; moves, so that installing an EFI bootloader is possible.  The user
       ;; is responsible for adding an entry for the EFI system partition to
       ;; the new system's fstab, but we are responsible for restoring
       ;; knowledge of the partition to the kernel's mount table.
       (when (remote-mount-point-p "/boot/efi")
         (destructuring-bind (type source options)
             (words (stripln (run "findmnt" "-nro" "FSTYPE,SOURCE,OPTIONS"
                                  "/boot/efi")))
           (setq efi-system-partition-mount-args
                 `("-t" ,type "-o" ,options ,source "/boot/efi"))))

       ;; /run is tricky because we want to retain the contents of the tmpfs
       ;; mounted there until reboot, for similar reasons to wanting to retain
       ;; /tmp, but unlike /tmp, /proc and /sys, a freshly debootstrapped
       ;; system contains a few things under /run and we would like to move
       ;; these out of /new-os.  So we temporarily relocate the /run mount.
       ;;
       ;; If this causes problems we could reconsider -- there's usually a
       ;; tmpfs mounted at /run, so those files underneath might not matter.
       (mrun "mount" "--make-private" "/")
       (system "mount" "--move" "/run" (ensure-directories-exist "/old-run/"))

       ;; We are not killing any processes, so lazily unmount everything
       ;; before trying to perform any renames.  (Present structure of this
       ;; loop assumes that each member of PRESERVED-DIRECTORIES is directly
       ;; under '/'.)
       ;;
       ;; We use system(3) to mount and unmount because once we unmount /dev,
       ;; there may not be /dev/null anymore, depending on whether the root
       ;; filesystems of the old and new OSs statically contain the basic /dev
       ;; entries or not, and at least on SBCL on Debian UIOP:RUN-PROGRAM
       ;; wants to open /dev/null when executing a command with no input.
       ;; Another option would be to pass an empty string as input.
       (loop with sorted = (cdr (mount:all-mounts)) ; drop '/' itself
             as next = (pop sorted)
             while next
             do (loop while (subpathp (car sorted) next) do (pop sorted))
             unless (preservedp next)
               do (system "umount" "--recursive" "--lazy" next))

       (let (done)
         (handler-case
             (flet ((rename (s d) (rename-file s d) (push (cons s d) done)))
               (dolist (file (local-directory-contents #P"/"))
                 (unless (or (preservedp file)
                             (pathname-equal file new-os)
                             (pathname-equal file old-os))
                   (rename file (chroot-pathname file old-os))))
               (dolist (file (local-directory-contents new-os))
                 (let ((dest (in-chroot-pathname file new-os)))
                   (unless (preservedp dest)
                     (when (or (file-exists-p dest) (directory-exists-p dest))
                       (failed-change
                        "~A already exists in root directory." dest))
                     (rename file dest)))))
           (serious-condition (c)
             ;; Make a single attempt to undo the moves to increase the chance
             ;; we can fix things and try again.
             (loop for (source . dest) in done do (rename-file dest source))
             (signal c))))
       (delete-directory-tree new-os :validate t)

       ;; Restore /run and any submounts, like /run/lock.
       (system "mount" "--move" "/old-run" "/run")
       (delete-empty-directory "/old-run")

       ;; For the freshly bootstrapped OS let's assume that HOME is /root and
       ;; XDG_CACHE_HOME is /root/.cache; we do want to try to read the old
       ;; OS's actual XDG_CACHE_HOME.  Move cache & update environment.
       (let ((source
               (chroot-pathname
                (merge-pathnames
                 "consfigurator/" (get-connattr :XDG_CACHE_HOME))
                old-os)))
         (when (directory-exists-p source)
           (rename-file source (ensure-directories-exist
                                #P"/root/.cache/consfigurator/"))))
       (setf (get-connattr :remote-user) "root"
             (get-connattr :remote-home) #P"/root/"
             (get-connattr :XDG_CACHE_HOME) #P"/root/.cache/"
             (get-connattr :consfigurator-cache) #P"/root/.cache/consfigurator/")
       (posix-login-environment 0 "root" "/root")

       ;; Remount (mainly virtual) filesystems that other properties we will
       ;; apply might require (esp. relevant for installing bootloaders).
       (dolist (mount mount:+linux-basic-vfs+)
         (unless (preservedp (ensure-directory-pathname (lastcar mount)))
           (apply #'system "mount" mount)))
       (when efi-system-partition-mount-args
         (ensure-directories-exist #P"/boot/efi/")
         (apply #'mrun "mount" efi-system-partition-mount-args))))))

(defproplist cleanly-installed-once :lisp
    (original-os-architecture
     &optional options
     &aux (minimal-new-host
	   (make-host :hostattrs (list :os (get-hostattrs :os))))
     (original-host
      (make-host
       :propspec
       (make-propspec
        :propspec
        `(eseqprops
          (os:linux ,original-os-architecture)
          (chroot:os-bootstrapped-for ,options "/new-os" ,minimal-new-host))))))
  "Replaces whatever operating system the host has with a clean installation of
the OS that the host is meant to have, and reboot, once.  This is intended for
freshly launched machines in faraway datacentres, where your provider has
installed some operating system image to get you started, but you'd like have
a greater degree of control over the contents and configuration of the
machine.  For example, this can help you ensure that the operation of the host
does not implicitly depend upon configuration present in the provider's image
but not captured by your consfig.  This property's approach can fail and leave
the system unbootable, but it's an time-efficient way to ensure that you're
starting from a truly clean slate for those cases in which it works.

ORIGINAL-OS-ARCHITECTURE is the architecture of the original OS as would be
supplied to the OS:LINUX property, e.g. :AMD64.  OPTIONS will be passed on to
CHROOT:OS-BOOTSTRAPPED-FOR, which see.

The internal property CHROOT::%OS-BOOTSTRAPPER-INSTALLED will attempt to use
PACKAGE:INSTALLED to install the OS bootstrapper (e.g. debootstrap(8) for
Debian).  Alternatively, you can install the bootstrapper manually before
running Consfigurator; this is useful for original OSs whose package managers
Consfigurator doesn't yet know how to drive.  You might apply an OS-agnostic
property before this one which manually downloads the bootstrapper and puts it
on PATH.

The files from the old OS will be left in '/old-os'.  Typically you will need
to perform some additional configuration before rebooting to increase the
likelihood that the system boots and is network-accessible.  This might
require copying information from '/old-os' and/or the kernel's state before
the reboot. Some of this will need to be attached to the application of this
property using ON-CHANGE, whereas other fixes can just be applied subsequent
to this property.  Here are two examples.  If you already know the machine's
network configuration you might use

    (os:debian-stable \"bullseye\" :amd64)
    (installer:cleanly-installed-once ...)
    (network:static \"ens3\" \"1.2.3.4\" ...)
    (file:has-content \"/etc/resolv.conf\" ...)

whereas if you don't have that information, you would want something like

    (os:debian-stable \"bullseye\" :amd64)
    (on-change (installer:cleanly-installed-once ...)
      (file:is-copy-of \"/etc/resolv.conf\" \"/old-os/etc/resolv.conf\"))
    (network:preserve-static-once)

Here are some other propapps you might want to attach to the application of
this property with ON-CHANGE:

    (bootloaders-installed)
    (fstab:has-entries-for-volumes
     (disk:volumes
       (mounted-ext4-filesystem :mount-point #P\"/\")
       (partition (mounted-fat32-filesystem :mount-point #P\"/boot/efi/\"))))
    (file:is-copy-of \"/root/.ssh/authorized_keys\"
                     \"/old-os/root/.ssh/authorized_keys\")
    (mount:unmounted-below-and-removed \"/old-os\")

You will probably need to install a kernel, bootloader, sshd etc. in the list
of properties subsequent to this one.  A more complete example:

    (os:debian-stable \"bullseye\" :amd64)
    (disk:has-volumes
     (physical-disk
      :device-file #P\"/dev/sda\"
      :boots-with '(grub:grub :target \"x86_64-efi\")))
    (on-change (installer:cleanly-installed-once
                nil '(os:debian-stable \"buster\" :amd64))
      ;; Clear out the old OS's EFI system partition contents.
      (file:directory-does-not-exist \"/boot/efi/EFI\")

      (apt:installed \"linux-image-amd64\")
      (installer:bootloaders-installed)

      (fstab:has-entries-for-volumes
       (disk:volumes
         (mounted-ext4-filesystem :mount-point #P\"/\")
         (partition
          (mounted-fat32-filesystem :mount-point #P\"/boot/efi/\"))))

      (file:is-copy-of \"/etc/resolv.conf\" \"/old-os/etc/resolv.conf\")
      (mount:unmounted-below-and-removed \"/old-os\"))
    (network:static ...)
    (sshd:installed)
    (swap:has-swap-file \"2G\")

If the system is not freshly provisioned, you couldn't easily recover from the
system becoming unbootable, or you have physical access to the machine, it is
probably better to use Consfigurator to build a disk image, or boot into a
live system and use Consfigurator to install to the host's usual storage."
  (:desc "OS cleanly installed once")
  (:hostattrs (os:required 'os:linux))
  (with-flagfile "/etc/consfigurator/os-cleanly-installed"
    (deploys :local original-host)
    (%root-filesystems-flipped "/new-os" "/old-os")
    ;; Prevent boot issues caused by disabled shadow passwords.
    (cmd:single "shadowconfig" "on")
    (reboot:at-end)))
