;;; Consfigurator -- Lisp declarative configuration management system

;;; Copyright (C) 2021-2022  Sean Whitton <spwhitton@spwhitton.name>

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

;;; The basic reason VOLUME-BOOTLOADERS is not just a propspec but a
;;; specification for a propspec is that we need to pass an OPENED-VOLUME to
;;; the property that will install the bootloader.  But maybe we could have
;;; INSTALL-BOOTLOADER-PROPSPEC but not INSTALL-BOOTLOADER-BINARIES-PROPSPEC.
;;; The installation of the bootloader binaries would always be done just
;;; before installing the bootloader, and the user would not need to
;;; separately apply BOOTLOADER-BINARIES-INSTALLED.

(defgeneric install-bootloader-propspec (bootloader-type volume &key)
  (:documentation
   "Return a propspec expression which installs bootloader of type
BOOTLOADER-TYPE to VOLUME.
The propapp yielded by the propspec may be of type :POSIX or of type :LISP.

The property can call CONTAINER:CONTAINED-P with relevant factors to determine
whether the host to which we are connected is the host the bootloader will
boot.  For example, (container:contained-p :efi-nvram) returns NIL when
building disk images, and T when installing a host from a live environment.
Bootloader installation might behave differently when certain factors are not
contained, or error out.  For examples, see GRUB:GRUB-INSTALLED and
U-BOOT:INSTALLED-ROCKCHIP."))

(defgeneric install-bootloader-binaries-propspec (bootloader-type volume &key)
  (:documentation
   "Return a propspec expression evaluating to a :POSIX propapp which
fetches/installs whatever binaries/packages need to be available to install
BOOTLOADER-TYPE to VOLUME."))

(defun get-propspecs (volumes)
  (loop for volume in (mapcan #'all-subvolumes volumes)
        when (slot-boundp volume 'volume-bootloaders)
          nconc (loop with bls = (volume-bootloaders volume)
                      for bootloader in (if (listp (car bls)) bls (list bls))
                      collect (destructuring-bind (type . args) bootloader
                                (apply #'install-bootloader-propspec
                                       type volume args)))))

;; At :HOSTATTRS time we don't have the OPENED-VOLUME values required by the
;; :APPLY subroutines which actually install the bootloaders.  So we call
;; GET-PROPSPECS twice: (in FILES-INSTALLED-TO-VOLUMES-FOR) at :HOSTATTRS time
;; to generate propspecs for the sake of running :HOSTATTRS subroutines, and
;; then at :APPLY time where we can get at the OPENED-VOLUME values, we ignore
;; the previously generated propspecs and call GET-PROPSPECS again.  This
;; approach should work for any sensible VOLUME<->OPENED-VOLUME pairs.
(define-function-property-combinator %install-bootloaders (&rest propapps)
  (:retprop
   :type :lisp
   :hostattrs (lambda () (mapc #'propapp-attrs propapps))
   :apply
   (lambda ()
     (mapc #'consfigure (get-propspecs (get-connattr 'disk:opened-volumes)))
     (mrun "sync"))))


;;;; Properties

(defprop %update-target-from-chroot :posix (chroot target)
  (:desc #?"Updated ${target} from ${chroot}")
  (:apply
   (assert-remote-euid-root)
   (run "rsync" "-PSavx" "--delete"
        (loop for volume
                in (mapcan (curry #'subvolumes-of-type 'mounted-filesystem)
                           (get-connattr 'disk:opened-volumes))
              collect (strcat "--exclude="
                              (unix-namestring (mount-point volume))))
        (strcat (unix-namestring chroot) "/")
        (strcat (unix-namestring target) "/"))))

(defun chroot-target (chroot)
  (ensure-directory-pathname
   (strcat
    (drop-trailing-slash (unix-namestring (ensure-directory-pathname chroot)))
    ".target")))

(defpropspec files-installed-to-volumes-for :lisp
    (options host volumes &key chroot leave-open
             (mount-below (if chroot (chroot-target chroot) #P"/target/")))
  "Where CHROOT contains the root filesystem of HOST and VOLUMES is a list of
volumes, recursively open the volumes and rsync in the contents of CHROOT.
Also update the fstab and crypttab, and try to install bootloader(s).

If CHROOT is NIL, bootstrap a root filesystem for HOST directly to VOLUMES.
In that case, OPTIONS is passed on to CHROOT:OS-BOOTSTRAPPED-FOR, which see.

MOUNT-BELOW and LEAVE-OPEN are passed on to WITH-OPENED-VOLUMES, which see."
  (:desc (format nil "~A installed to volumes"
                 (or chroot (get-hostname host))))
  `(with-opened-volumes
       (,volumes :mount-below ,mount-below :leave-open ,leave-open)
     ,(if chroot
          `(%update-target-from-chroot ,chroot ,mount-below)
          `(chroot:os-bootstrapped-for ,options ,mount-below ,host))
     (chroot:deploys-these
      ,mount-below ,host
      ,(make-propspec
        :propspec
        `(eseqprops
          ,(propapp
            (os:etypecase
              (debianlike
               (file:lacks-lines
                "/etc/fstab" "# UNCONFIGURED FSTAB FOR BASE SYSTEM")
               ;; These will overwrite any custom mount options, etc., with
               ;; values from VOLUMES.  Possibly it would be better to use
               ;; properties which only update the fs-spec/source fields.
               ;; However, given that VOLUMES ultimately comes from the
               ;; volumes the user has declared for the host, it is unlikely
               ;; there are other properties setting mount options etc. which
               ;; are in conflict with VOLUMES.
               (fstab:has-entries-for-opened-volumes)
               (crypttab:has-entries-for-opened-volumes))))
          (%install-bootloaders
           ,@(get-propspecs (get-hostattrs :volumes))))))))

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

(defpropspec bootloaders-installed :lisp ()
  "Install the host's bootloaders to its volumes.
Intended to be attached to properties like INSTALLER:CLEANLY-INSTALLED-ONCE
using a combinator like ON-CHANGE, or applied manually with DEPLOY-THESE."
  (:desc "Bootloaders installed")
  `(eseqprops
    (bootloader-binaries-installed)
    ,@(get-propspecs (get-hostattrs :volumes))))


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

(defproplist %cleanly-installed-once :lisp
    (options original-os
             &aux
             (new (make-host :hostattrs `(:os ,(get-hostattrs :os))))
             (original-host
              (make-host
               :propspec
               (make-propspec
                :propspec
                `(eseqprops
                  ,(or original-os '(os:linux))
                  (chroot:os-bootstrapped-for ,options "/new-os" ,new))))))
  (with-flagfile "/etc/consfigurator/os-cleanly-installed"
    (deploys :local original-host)
    (%root-filesystems-flipped "/new-os" "/old-os")
    ;; Prevent boot issues caused by disabled shadow passwords.
    (cmd:single "shadowconfig" "on")))

(defproplist cleanly-installed-once :lisp (&optional options original-os)
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

OPTIONS will be passed on to CHROOT:OS-BOOTSTRAPPED-FOR, which see.
ORIGINAL-OS, if supplied, is a propapp specifying the old OS, as you would
apply to a host with that OS.

The internal property CHROOT::%OS-BOOTSTRAPPER-INSTALLED will attempt to
install the OS bootstrapper (e.g. debootstrap(8) for Debian).  If ORIGINAL-OS
is supplied then installation will use a package manager property for that OS.
Otherwise, CHROOT::%OS-BOOTSTRAPPER-INSTALLED will fall back to trying
PACKAGE:INSTALLED.  Alternatively, you can install the bootstrapper manually
before running Consfigurator and not supply ORIGINAL-OS.  This is useful for
original OSs whose package managers Consfigurator doesn't yet know how to
drive.  You might apply an OS-agnostic property before this one which manually
downloads the bootstrapper and puts it on PATH.

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

You will probably need to install a kernel, bootloader, sshd etc. in the list
of properties subsequent to this one.  A more complete example, using the
combinator INSTALLER:WITH-CLEANLY-INSTALLED-ONCE, which see:

    (os:debian-stable \"bullseye\" :amd64)
    (disk:has-volumes
     (physical-disk
      :device-file #P\"/dev/sda\"
      :boots-with '(grub:grub :target \"x86_64-efi\")))
    (installer:with-cleanly-installed-once
        (nil '(os:debian-stable \"buster\" :amd64))

      :post-install
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
      (mount:unmounted-below-and-removed \"/old-os\")

      :always
      (network:static ...)
      (sshd:installed)
      (swap:has-swap-file \"2G\"))

Here are some other propapps you might want in the :POST-INSTALL list:

    (bootloaders-installed)
    (fstab:has-entries-for-volumes
     (disk:volumes
       (mounted-ext4-filesystem :mount-point #P\"/\")
       (partition (mounted-fat32-filesystem :mount-point #P\"/boot/efi/\"))))
    (file:is-copy-of \"/root/.ssh/authorized_keys\"
                     \"/old-os/root/.ssh/authorized_keys\")
    (mount:unmounted-below-and-removed \"/old-os\")

If the system is not freshly provisioned, you couldn't easily recover from the
system becoming unbootable, or you have physical access to the machine, it is
probably better to use Consfigurator to build a disk image, or boot into a
live system and use Consfigurator to install to the host's usual storage."
  (:desc "OS cleanly installed once")
  (:hostattrs (os:required 'os:linux))
  (on-change (%cleanly-installed-once options original-os) (reboot:at-end)))

(defmacro with-cleanly-installed-once
    ((&optional options original-os) &body propapps)
  "Apply INSTALLER:CLEANLY-INSTALLED-ONCE, passing along OPTIONS and
ORIGINAL-OS, and attach to that application, using other property combinators,
the application of PROPAPPS.

PROPAPPS is a concatenation of three lists of propapps delimited by keywords:

    '(:post-install
      (propapp1) (propapp2) ...

      :always
      (propapp3) (propapp4) ...

      :post-first-reboot
      (propapp5) (propapp6) ...)

Any of the keywords and their propapps may be absent, but the three lists must
appear in this order.  The :POST-INSTALL propapps are applied only if this
deployment performed the clean reinstallation, right after that.  The :ALWAYS
propapps are applied next, whether or not this deployment performed the clean
reinstallation.  Finally, the :POST-FIRST-REBOOT propapps are applied, only if
this deployment did not perform the clean reinstallation.

This mechanism handles common usages of INSTALLER:CLEANLY-INSTALLED-ONCE.  For
example:

    (installer:with-cleanly-installed-once (...)
      :post-install
      (installer:bootloaders-installed)
      (file:is-copy-of \"/etc/resolv.conf\" \"/old-os/etc/resolv.conf\")
      (mount:unmounted-below-and-removed \"/old-os\")

      :always
      (apt:installed \"openssh-server\")
      (ssh:authorized-keys ...)
      (network:static \"enp1s0\" ...)

      :post-first-reboot
      (my-cool-web-service)
      (apache:https-vhost ...))

Properties that should be applied only once, or that rely on accessing files
from /old-os, are applied under :POST-INSTALL.  Networking and shell access
are established before the first reboot, so we don't lock ourselves out.
However, as these properties are part of the usual definition of the host,
they go under :ALWAYS, not :POST-INSTALL, so that Consfigurator checks they
are still applied each deployment.  Finally, we defer setting up the host's
sites and services until after the first reboot, in case there are any
problems setting those up when it's still the old OS's kernel that's running."
  `(with-cleanly-installed-once*
       (%cleanly-installed-once ,options ,original-os) ,@propapps))

(define-function-property-combinator with-cleanly-installed-once*
    (cleanly-installed-once-propapp &rest propapps)
  (let* ((post-first-reboot (member :post-first-reboot propapps))
         (always-kw (member :always propapps))
         (always (ldiff always-kw post-first-reboot))
         (post-install (ldiff (member :post-install propapps)
                              (or always-kw post-first-reboot)))

         (post-install (apply #'eseqprops (cdr post-install)))
         (always-before (apply #'eseqprops (cdr always)))

         ;; Use SEQPROPS like DEFHOST does.
         (always-after (apply #'seqprops (cdr always)))
         (post-first-reboot (apply #'seqprops (cdr post-first-reboot))))
    (:retprop :type :lisp
              :hostattrs (lambda-ignoring-args
                           (propapp-attrs cleanly-installed-once-propapp)
                           (propapp-attrs post-install)
                           (propapp-attrs always-after)
                           (propapp-attrs post-first-reboot))
              :apply (lambda-ignoring-args
                       (case (apply-propapp cleanly-installed-once-propapp)
                         (:no-change
                          (prog-changes
                            (add-change (apply-propapp always-after))
                            (add-change (apply-propapp post-first-reboot))))
                         (t
                          (apply-propapp post-install)
                          (apply-propapp always-before)
                          (reboot:at-end)
                          nil))))))
