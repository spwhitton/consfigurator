Tutorial: OS installation
=========================

Consfigurator implements at least the basic elements of a number of methods
for installing operating systems.

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

Live replacement of provider cloud images
-----------------------------------------

See the docstring of the INSTALLER:CLEANLY-INSTALLED-ONCE property.  This is
an efficient way to handle machines in faraway datacentres.  Consfigurator's
support for installing Debian stable this way has been fairly well tested, and
the technique should work for other operating systems too, once Consfigurator
has been taught how to bootstrap them.

Build a specialised live image
------------------------------

This third approach is more experimental; Consfigurator has all the necessary
capabilities, at least for Debian, but at present you'll need to string them
together yourself in your consfig.  With this approach you build a live image
containing everything you need to run Consfigurator on the hardware to which
you want to install.  After booting up the live system, you can either run
Consfigurator manually, or you can set things up to have it run automatically
upon boot.

Consfigurator's ability to bootstrap fresh root filesystems typically requires
Internet access, but an alternative is to build and customise a chroot
corresponding to the root filesystem of the target system, and include that in
the live image, such that after boot Consfigurator just needs to partition the
disk, copy in the contents of the prebuilt chroot, and update /etc/fstab and
/etc/crypttab with UUIDs.  Here is a sketch of how to do something like that::

    (try-register-data-source
     :git-snapshot :name "consfig"
     :repo #P"src/cl/consfig/" :depth 1 :branch "master")

    (defproplist live-installer-built-for :lisp (with-chroot-for)
      "Build a custom Debian Live system at /srv/live/installer.iso.

    Typically this property is not applied in a DEFHOST form, but rather run as
    needed at the REPL.  The reason for this is that otherwise the whole image will
    get rebuilt each time a commit is made to ~/src/cl/consfig/."
      (:desc "Debian Live system image built")
      (disk:debian-live-iso-built. nil "/srv/live/installer.iso"
        (os:debian-stable "bullseye" :amd64)
        (apt:installed "task-english" "live-config" "lvm2" "cryptsetup")
        (git:snapshot-extracted "/etc/skel/src/cl" "consfig")
        (chroot:os-bootstrapped-for
         nil
         (merge-pathnames (get-hostname with-chroot-for) "/srv/chroot/")
         with-chroot-for)))

Supposing we've a DEFHOST form for test.silentflame.com, on our laptop we
could then use::

  CONSFIG> (hostdeploy-these laptop.silentflame.com
             (live-installer-built-for test.silentflame.com))

Then once the live system has booted on the target host, you'd use the
DISK:HOST-VOLUMES-CREATED and INSTALLER:CHROOT-INSTALLED-TO-VOLUMES properties
to complete the installation.

To prepare a live image that is capable of installing more than one system
without an Internet connection, you'd probably need to investigate including
an apt repo, or equivalent, in the live system, and point Consfigurator's OS
bootstrapping properties at that.
