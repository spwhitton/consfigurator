Tutorial: building disk images
==============================

In this tutorial we will show you what properties you need to use to build
bootable disk images.

.. include:: conventions.rst

Consfiguration
--------------

Here is a minimal definition of the host for which we can build a disk image:::

  (defhost test.example.com ()
    (os:debian-stable "bullseye" :amd64)
    (disk:has-volumes
     (physical-disk
      :device-file #P"/dev/sda"
      :boots-with '(grub:grub :target "x86_64-efi" :force-extra-removable t)
      (partitioned-volume
       ((partition
         :partition-typecode #xEF00
         (fat32-filesystem :volume-size 512 :mount-point #P"/boot/efi/"))
        (partition
         (ext4-filesystem :extra-space 400 :mount-point #P"/"))))))
    (installer:bootloader-binaries-installed)
    (apt:installed "linux-image-amd64")
    (user:has-enabled-password "root"))

- The ``DISK:HAS-VOLUMES`` property is like the ``OS:DEBIAN-STABLE`` property
  in that both simply set hostattrs on the host -- they establish metadata to
  which other properties may refer.  In this case, we specify that the machine
  has a single physical disk with two partitions, and that it boots with GRUB.
  We also request that Consfigurator pass the ``--force-extra-removable`` flag
  to grub-install(8), because that makes it a bit easier to test our image,
  given how UEFI works.

  We've requested 400M of free space on the root partition beyond whatever the
  base system install takes up.  For the EFI system partition we specify an
  absolute size.

- The ``INSTALLER:BOOTLOADER-BINARIES-INSTALLED`` property reads the metadata
  established by ``DISK:HAS-VOLUMES`` and ensures that binaries like
  grub-install(8) are available.  In this case you could replace it with just
  ``(apt:installed "grub-efi-amd64")``, but using this property avoids
  repeating yourself.

- Finally, building a bootable image requires installing a kernel.

Building the image
------------------

What we've established so far is a definition of a host.  But it does not yet
make any sense to say ``(deploy :foo test.example.com ...)`` because the
host does not yet exist anywhere for us to connect to it.  What we can now use
is the ``DISK:RAW-IMAGE-BUILT-FOR`` property, which we can apply to a host
which *does* already exist to build an image for our host which does not yet
exist:::

  CONSFIG> (hostdeploy-these laptop.example.com
             (disk:raw-image-built-for
	      nil test.example.com "/home/spwhitton/tmp/test.img"))

This property does the following on laptop.example.com:

1. Build a chroot with the root filesystem of test.example.com, and apply
   all its properties, such as installing the kernel and building the
   initramfs.

2. Transform the metadata set by ``DISK:HAS-VOLUMES`` such that the instance
   of ``PHYSICAL-DISK`` is replaced with an instance of ``RAW-DISK-IMAGE``,
   and then make, partition and mount the image file, using tools like
   kpartx(8).

3. Rsync the contents of the chroot into the mounted partitions.

4. Update /etc/fstab so that it contains the UUIDs of the partitions.

5. Install the bootloader(s), again as specified by ``DISK:HAS-VOLUMES``.

Here we've described this procedurally, but the semantics of
``DISK:RAW-IMAGE-BUILT-FOR`` are declarative, like all properties.  You can
add the property to the ``DEFHOST`` for your laptop and Consfigurator will
just do whatever is needed to keep the chroot and the disk image up-to-date
(though see the docstring for that property for some limitations).

All of this is modular: take a look in ``src/property/disk.lisp`` to see how
new volume types can be defined, and in ``src/property/grub.lisp`` and
``src/property/u-boot.lisp`` to see how Consfigurator can be taught to install
different bootloaders.

Testing the image
-----------------

Here's a quick way to test what we've built:::

  % sudo chown $USER tmp/test.img
  % qemu-system-x86_64 -m 2G -drive file=tmp/test.img,format=raw \
      -drive "if=pflash,format=raw,readonly=on,file=/usr/share/OVMF/OVMF_CODE.fd"

It should boot up and you can login as root, password "changeme".

Uses for the disk image
-----------------------

You might upload this image to a cloud provider and boot up a minimal
instance.  Supposing we also added at least an sshd and our public key, we
could then continue to add properties to the ``DEFHOST`` for
test.example.com and then apply them with SSH:::

  CONSFIG> (deploy ((:ssh :user "root") :sbcl) test.example.com)

Another possibility is to dd the image out to a USB flash drive and then boot
a physical machine from it.

It should be straightforward to adapt the existing code to have Consfigurator
install a bootable system to a physical volume rather than to a disk image,
but the high level properties for this haven't been written yet.
