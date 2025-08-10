News
====

Some user-visible changes are documented here.  Version numbers have three
components, ``major.minor.patch``, with the following semantics:

- we increment ``major`` for a release which contains particularly significant
  new features, enhancements and/or reworkings, whether or not upgrading to
  the release will require changes to user consfigs (though usually it will);

- we increment ``minor`` for a release which does not satisfy the requirements
  for incrementing ``major``, but where there are changes that could require
  changes in user consfigs, except very obscure such consfigs; and

- we increment only ``patch`` for a release which includes no changes that we
  think could require changes in user consfigs.

Note that the notion of consfig-breaking changes is more than just strict API
breaks, but also changes in behaviour which will likely require review by
sysadmins using Consfigurator to maintain their systems.

In summary, you should always be able to upgrade to a release which only
increments ``patch``, but if either of the other two components have changed,
you should review this document and see if your consfig needs updating.

1.5.3 (unreleased)
------------------

- Adapt WRITE-REMOTE-FILE to handle BusyBox's ls(1).
  Thanks to David Bremner.

1.5.0 (2025-03-29)
------------------

- API change: APT:SUITES-AVAILABLE-PINNED now requires that you also apply
  APT:STANDARD-SOURCES.LIST to the host, or it adds only a pin, but no source.

- APT properties now generate deb822-style sources configuration files.

- APT:ADDITIONAL-SOURCES can now install deb822-style format files.
  It can still install traditional one-line-style format files, too.

- New APT:ADDITIONAL-SUITES property.

- GIT:INSTALLED: Add a :CHECK subroutine.  This makes properties like
  GIT:CLONED usable by non-root in the case that Git is already installed.

- New ``LOCALSUDON`` macro, like ``LOCALSUDO`` except it assumes sudo has been
  configured not to ask for a password.

- CHROOT:OS-BOOTSTRAPPED and CHROOT:OS-BOOTSTRAPPED-FOR: When bootstrapping a
  Debian system on a Debian-like system, in most cases use mmdebstrap(1), not
  debootstrap(8).  Applying these properties still requires root.

- New POSIX-LEFT-TRIM, POSIX-RIGHT-TRIM, POSIX-TRIM utilities.

1.4.4 (2024-10-10)
------------------

- APT:UPDATED: Continue to try to execute ``apt-get update`` even if
  ``dpkg --configure --pending`` fails.

- All APT properties: Pass ``-o DPkg::Lock::Timeout=60`` to apt-get(8).

1.4.1 (2024-07-10)
------------------

- DISK::CREATE-VOLUME for DISK:PARTITIONED-VOLUME now executes
  ``sgdisk --clear`` before ``sgdisk --zap-all`` to avoid certain failures to
  clear out the block device.

- Add OS types for FreeBSD.

- Add some properties for FreeBSD's pkg(8) and rc.conf(5).

- :SBCL Lisp-type connections can now be established to FreeBSD hosts.

1.4.0 (2024-05-09)
------------------

- APACHE:HTTPS-VHOST now adds an Apache ``<Directory>`` directive which
  ensures that HTTP access to the ``.well-known/acme-challenge/`` subdirectory
  of the document root is granted.

- APT:STANDARD-SOURCES-FOR will not try to add *-backports sources for stable
  releases whose *-backports dists are gone from the official Debian mirrors.

1.3.2 (2024-04-24)
------------------

- Add LIBVIRT:KVM-BOOTS-LVM-LV and LIBVIRT:KVM-BOOTS-LVM-LV-FOR.

- Pass ``--batch`` and ``--no-tty`` to gpg(1) to avoid some tty issues.
  Thanks to David Bremner.

- Fix a bug in DISK:HOST-LOGICAL-VOLUMES-EXIST that meant it would try to add
  ``/etc/fstab`` entries for each logical volume it created rather than for
  each filesystem it tries to mount.

- FILE:HOST-DATA-UPLOADED and FILE:HOST-SECRET-UPLOADED can now upload
  multiple files.

1.3.1 (2023-06-12)
------------------

- Add REAPPLIED and PERIODIC:REAPPLIED-AT-MOST property combinators.

1.3.0 (2023-03-17)
------------------

- Readtable:

  - New reader macros ``#~m//`` and ``#~s///`` for shell- and Perl-style
    regular expression matching and replacement.

  - New reader macro ``#>>EOF>>`` which is like ``#>EOF>`` except that it
    skips over the remainder of the current line and its newline.  This is
    more like how heredocs work in other languages.

  - Support for indented heredocs, where the indentation of the lines of the
    heredoc is stripped.  This mode is activated by prepending a tilde to the
    heredoc terminator.  For example:

    .. code-block:: none

      (foo "argument 1" #>>~EOF>>
	   My line 1.
	   My line 2.
	   EOF)

    The function receives ``"My line 1.\nMy line 2."``

    This is a minor breaking change because heredoc terminators may no longer
    begin with a tilde.

- Documentation:

   - New manual section "Reader macros" discussing Consfigurator's named
     readtable, including some usage reservations for the sake of future
     extension.

   - New tutorial, "Defining new properties".

   - Extract docstrings and use them to generate API references in the manual.
     It should now be possible to know what properties are available for your
     use without having to read the source of properties modules.

- Miscellaneous:

   - New Emacs major mode, ``consfigurator-lisp-mode``.  This takes care of
     informing Emacs that parts of the buffer are CL-INTERPOL, CL-HEREDOC and
     our ``#~m//`` and ``#~s///`` strings, fixing SLIME's C-c C-c in certain
     cases.

1.2.2 (2023-02-20)
------------------

- APT properties: add ``non-free-firmware`` section to generated sources
  lists.  On Debian bullseye and older this will cause apt updates to emit
  harmless warnings.

- LXC:USER-CONTAINER and LXC:USER-CONTAINER-FOR are now unapplicable.

- Add LXC:USER-CONTAINER-STOPPED and FILE:EMPTY-DIRECTORY-DOES-NOT-EXIST.

1.2.0 (2022-11-29)
------------------

- APT properties now cache what packages have been explicitly installed and
  removed this deployment.  This should significantly speed up many
  deployments.  User properties which install or remove packages by calling
  apt-get(8) or dpkg(1) directly might inadvertedly render the lists of
  properties installed and removed invalid.  Such properties should be changed
  to call or apply the new APT:KNOWN-INSTALLED-REMOVED-PACKAGES-RESET property
  to invalidate the cache.

- Fix a bug in FILE:SYMLINKED that meant that with at least GNU ln(1), the
  property would fail to overwrite existing symbolic links in some cases.

- Fix a bug in FILE:DOES-NOT-EXIST which meant that it did nothing if only
  some of the files that should not exist needed to be deleted.

- When recovering from a failed debootstrap, instead of just recursively
  deleting the target directory, we now call EMPTY-REMOTE-DIRECTORY, to empty
  it, instead.  This is better when the target directory is a mount point.

1.1.1 (2022-09-18)
------------------

- Add FILE:CONTAINS-CONF-UNSPACED.

- PROPAPPLY now signals an error if asked to apply a property that has neither
  ``:APPLY`` nor ``:HOSTATTRS`` subroutines.  This is primarily intended to
  catch cases where the property is defined in a ``.lisp`` file that you
  haven't yet added to your consfig's ``.asd`` file.

- APT:PROXY is now unapplicable.

- APT:INSTALLED, APT:INSTALLED-MINIMALLY and APT:REMOVED now always execute
  apt-get(8).  Previously they tried to determine whether the packages were
  already installed or removed by parsing output from apt-cache(8), but the
  implementation sometimes gave the wrong answer.

1.1.0 (2022-08-02)
------------------

- API change: DISK:HOST-VOLUMES-CREATED has been removed in favour of new
  properties DISK:FIRST-DISK-INSTALLED-FOR and DISK:VOLUMES-INSTALLED-FOR.

- API change: INSTALLED:CHROOT-INSTALLED-TO-VOLUMES-FOR has been renamed to
  INSTALLER:FILES-INSTALLED-TO-VOLUMES-FOR, and will now bootstrap a root
  filesystem directly to the volumes if not supplied a chroot.  The CHROOT
  parameter has become a keyword parameter, and the required parameters have
  changed from ``(HOST CHROOT VOLUMES)`` to ``(OPTIONS HOST VOLUMES)`` for
  consistency with other property lambda lists.

  The new property also includes a bugfix: we now rebuild the initramfs after
  populating the crypttab.

- API change: DISK:WITH-OPENED-VOLUMES now includes volumes that were already
  open, and their parents, in the connattrs.

- DISK:LUKS-CONTAINER: Add support for passing arbitrary options to
  cryptsetup(8) when creating volumes, such as ``--cipher``.

- DISK:WITH-OPENED-VOLUMES, INSTALLER:FILES-INSTALLED-TO-VOLUMES-FOR and
  DISK:VOLUMES-INSTALLED-FOR support a new ``LEAVE-OPEN`` argument to request
  that opened volumes are not closed.  This is useful for inspecting the
  result of an installation, but must be used with caution: the next
  deployment will assume the volumes have been manually closed.

1.0.3 (2022-06-29)
------------------

- Wrap calls to OSICAT:USER-INFO with a fallback to use getent(1).  This fixes
  cases where getpwnam(3) and getpwuid(3) can fail to load required NSS modules
  because we have chrooted or similar.

- Consfigurator now converts some of its internal shell script snippets to
  single lines before executing them, which improves debug output and the
  readability of process names visible to remote commands like ps(1).

- Add PROG-CHANGES, USER:GROUP-EXISTS and INSTALLER:WITH-CLEANLY-INSTALLED-ONCE.

- ESEQPROPS-UNTIL can now be used with any condition class, not just those
  subtyping FAILED-CHANGE.

- REBOOT:AT-END now falls back to shell ``sleep``-based scheduling when
  shutdown(8) cannot schedule a reboot for the future.

- Fix a few bugs in FILE:CONTAINS-INI-SETTINGS.

- Fix FSTAB:HAS-ENTRIES-FOR-OPENED-VOLUMES for FAT32 filesystems.

1.0.1 (2022-05-11)
------------------

- Some enhancements to OS combinators.

- New data source to fetch passphrases from a `pass(1)`_ data store, thanks to
  David Bremner.

.. _pass(1): https://www.passwordstore.org/
