Ideas for future development
============================

Patches welcome.

Properties
----------

- Custom Emacs indentation rules for ``DISK:VOLUMES`` (and so
  ``DISK:HAS-VOLUMES``), and reindent the examples in docstrings and manual.

Connections
-----------

- Basic infrastructure for connections which work with just input and output
  streams connected to an interactive POSIX sh somewhere, like TRAMP, and
  probably using ``base64 -d`` for WRITE-REMOTE-FILE.  Probably the basic
  connection type will take a command to start up the shell as a keyword
  argument, and then we can have more specific connection types which take
  other arguments and construct the full command.

- It might be possible to write an implementation of CONNECTION-UPLOAD for
  SSH-CONNECTION which can optimise a common case.  If it can see that it is
  the only item in the connection chain, and there is an old version of an
  item of prerequisite data to upload already on the remote side, it can move
  that old version to a temporary name, rsync the new version directly to the
  temporary name so that rsync can do an incremental update, and then rename
  the file to the new version.

- It would sometimes be useful to have the SSH connection pass
  ``-oHostName=<known IP address>`` when ``NETWORK:IPV4`` and/or
  ``NETWORK:IPV6`` have been specified for the host, so that DNS propagation
  is less likely to get in the way of configuring the host.  Some hosts' SSH
  daemons might only be accessible over VPNs and the like, however, so it will
  need to be easy to override this.

- Perhaps ``:SSH`` ought to enable connection sharing (including
  ``ControlPersist``) on the user's behalf, rather than relying on users
  adding this to ``~/.ssh/config``.

Core
----

- We signal a warning whenever the contents of a property's function cell is
  called when that property has a :HOSTATTRS subroutine.  Could we figure out
  catching and muffling the warning when the :HOSTATTRS subroutine did get
  run?

- A CONCURRENTLY combinator for property application specifications, which
  means to apply each of the enclosed properties in parallel.  Particularly
  useful surrounding a set of DEPLOYS applications, to concurrently deploy a
  number of hosts.  Now that we don't call fork(2) while executing
  deployments, we ought to be able to do this using threads, and so it can
  work in the root Lisp too.  However, we still use ``WITH-CURRENT-DIRECTORY``
  in various places, and temporarily set HOME in ``WITH-HOMEDIR``.  Perhaps
  ``WITH-CURRENT-DIRECTORY`` could be changed to only affect RUN, MRUN
  etc. for the sake of enabling multithreading.

- It might be useful to have a restart for the case where an attempt is made
  to apply a list of properties containing some ``:LISP`` properties with a
  POSIX-type connection which applies properties up to but not including the
  first ``:LISP`` property in the sequence, to get as much work as possible
  done without violating any dependency relationships (``SEQPROPS`` already
  handles wanting to apply all of the ``:POSIX`` properties in the sequence).
  But maybe this is unnecessarily complex -- wouldn't it be better to just
  fail and fix your deployment definitions?

- Combinator WITH-REQUIREMENTS-FOR-CHANGE to only apply dependencies if the
  first property's :CHECK routine indicates that a change it needed.  For
  example, if the chroot already exists, we don't attempt to install
  debootstrap.  (The ``SBCL-AVAILABLE`` property in
  ``src/connection/sbcl.lisp`` achieves a similar effect in another way.)

- We might want ``DEPLOYS``, ``DEPLOYS-THESE`` to push the host object to the
  parent host's hostattrs, under ``:CHILD-HOSTS`` or something.  This means
  that hosts implicitly defined inline using dotted propapp notation are
  accessible via their parents, so that a property which gathers up DNS
  information about all hosts, for example, would be able to find them.

- At debug level 2, when WRITE-REMOTE-FILE makes a change, it could print a
  diff/patch to show what it did.
