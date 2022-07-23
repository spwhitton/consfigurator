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

1.1.0 (unreleased)
------------------

- API change: DISK:HOST-VOLUMES-CREATED has been removed in favour of new
  properties DISK:FIRST-DISK-INSTALLED-FOR and DISK:VOLUMES-INSTALLED-FOR.

- API change: INSTALLED:CHROOT-INSTALLED-TO-VOLUMES-FOR has been renamed to
  INSTALLER:FILES-INSTALLED-TO-VOLUMES-FOR, and will now bootstrap a root
  filesystem directly to the volumes if not supplied a chroot.  The CHROOT
  parameter has become a keyword parameter, and the required parameters have
  changed from ``(HOST CHROOT VOLUMES)`` to ``(OPTIONS HOST VOLUMES)`` for
  consistency with other property lambda lists.

- API change: DISK:WITH-OPENED-VOLUMES now includes volumes that were already
  open, and their parents, in the connattrs.

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
