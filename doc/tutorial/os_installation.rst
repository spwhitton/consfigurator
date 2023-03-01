OS installation
===============

Consfigurator implements a number of methods for installing operating systems.

.. include:: conventions.rst

Build and write out a raw disk image
------------------------------------

This is the simplest method, and Consfigurator has decent support built-in:
see the previous tutorial.  It is less practical for systems which have large
disks and/or complex, nested partitioning schemes, such as ext4 on LVM on
LUKS, as is common for GNU/Linux laptops.  In such cases it is nontrivial to
expand the partitions to fill the whole physical disk after the first
successful boot, so the disk image has to be the same size as the target disk,
which can be unwieldy.

Installing directly to target host's primary storage
----------------------------------------------------

This is similar to the previous method, but it avoids the problem of having to
expand filesystems upon first boot.  Suppose we have connected the target
host's hard disk to our laptop, or transferred its boot SD card to our
laptop's SD card reader, etc..  Then we can install to it directly:::

  CONSFIG> (hostdeploy-these laptop.example.com
             (disk:first-disk-installed-for nil test.example.com "/dev/mmcblk0"))

Live replacement of provider cloud images
-----------------------------------------

See the docstring of the INSTALLER:CLEANLY-INSTALLED-ONCE property.  This is
an efficient way to handle machines in faraway datacentres.  Consfigurator's
support for installing Debian stable this way has been fairly well tested, and
the technique should work for other operating systems too, once Consfigurator
has been taught how to bootstrap them.

Build a specialised live image
------------------------------

This fourth approach requires more work in your consfig.  With this approach
you build a live image containing everything you need to run Consfigurator on
the hardware to which you want to install.  After booting up the live system,
you can either run Consfigurator manually, or you can set things up to have it
run automatically upon boot.

Consfigurator's ability to bootstrap fresh root filesystems typically requires
Internet access, but an alternative is to build and customise a chroot
corresponding to the root filesystem of the target system, and include that in
the live image, such that after boot Consfigurator just needs to partition the
disk, copy in the contents of the prebuilt chroot, and update /etc/fstab and
/etc/crypttab with UUIDs.  Here is a minimal version of something like that::

    (try-register-data-source
     :git-snapshot :name "consfig"
     :repo #P"common-lisp/consfig/" :depth 1 :branch "master")

    (defproplist live-installer-built-for :lisp (with-chroot-for)
      "Build a custom Debian Live system at /srv/live/installer.iso.

    Typically this property is not applied in a DEFHOST form, but rather run as
    needed at the REPL.  The reason for this is that otherwise the whole image will
    get rebuilt each time a commit is made to ~/common-lisp/consfig/."
      (:desc "Debian Live system image built")
      (disk:debian-live-iso-built. nil "/srv/live/installer.iso"
        (os:debian-stable "bullseye" :amd64)
	(hostname:configured "debian")
        (apt:installed "sudo" "task-english" "emacs"
	               "sbcl" "slime" "cl-consfigurator" "build-essential"
	               "lvm2" "cryptsetup" "gdisk" "kpartx" "dosfstools")

	(user:has-account "user")
	(user:has-enabled-password "user" :initial-password "live")
	(file:exists-with-content "/etc/sudoers.d/user"
	  '("user  ALL=(ALL:ALL) NOPASSWD: ALL") :mode #o600)
        (as "user" (git:snapshot-extracted "consfig" "common-lisp/" :replace t))

        (chroot:os-bootstrapped-for
         nil
         (merge-pathnames (get-hostname with-chroot-for) "/srv/chroot/")
         with-chroot-for)))

Supposing we've a DEFHOST form for test.example.com, on our laptop we
could then use::

  CONSFIG> (hostdeploy-these laptop.example.com
             (live-installer-built-for test.example.com))

Then once the live system has booted on the target host, you'd complete the
installation using something like this:::

  CONSFIG> (deploy-these (:sudo :sbcl) "debian"
             (os:debian-stable "bullseye" :amd64)
             (disk:volumes-installed-for nil test.example.com :leave-open t
					 :chroot "/srv/chroot/test.example.com")

This approach requires that a ``:DEVICE-FILE`` is specified for each of the
host's physical disks.

To prepare a live image that is capable of installing more than one system
without an Internet connection, you'd probably need to investigate including
an apt repo, or equivalent, in the live system, and point Consfigurator's OS
bootstrapping properties at that.
