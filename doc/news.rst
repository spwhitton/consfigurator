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
