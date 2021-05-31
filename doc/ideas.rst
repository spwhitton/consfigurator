Ideas for future development
============================

Patches welcome.

Properties
----------

- Custom Emacs indentation rules for ``DISK:VOLUMES`` (and so
  ``DISK:HAS-VOLUMES``), and reindent the examples in docstrings and manual.

Connections
-----------

- :SBCL could (fork and) SAVE-LISP-AND-DIE.  That way, we have something that
  a cronjob can call to re-run the deployment to ensure that all properties
  remain applied.  Need to think about how the property which sets up the
  cronjob will be specified in consfigs -- does it make sense to allow passing
  in arbitrary deployments, or do we only allow re-running exactly the same
  thing?  If the former, the saved image will need to take some sort of
  command line input telling it what arguments to pass to DEPLOY*.

- Basic infrastructure for connections which work with just input and output
  streams connected to an interactive POSIX sh somewhere, like TRAMP, and
  probably using ``base64 -d`` for WRITEFILE.  Probably the basic connection
  type will take a command to start up the shell as a keyword argument, and
  then we can have more specific connection types which take other arguments
  and construct the full command.

- It might be possible to write an implementation of CONNECTION-UPLOAD for
  SSH-CONNECTION which can optimise a common case.  If it can see that it is
  the only item in the connection chain, and there is an old version of an
  item of prerequisite data to upload already on the remote side, it can move
  that old version to a temporary name, rsync the new version directly to the
  temporary name so that rsync can do an incremental update, and then rename
  the file to the new version.

Data sources
------------

- It might be useful to have a data source which can just provide a single
  item of data when registered.  Then in your consfig you can just register
  this data source to make a particular file you have on your system available
  to deployments.

Core
----

- Could we signal a condition whenever the contents of a property's function
  cell is called when that property has a :HOSTATTRS subroutine?  The point
  would be to avoid calling the property within another property without
  calling its :HOSTATTRS subroutine too -- could we figure out catching and
  ignoring the condition when its :HOSTATTRS subroutine did get run?

- A CONCURRENTLY combinator for property application specifications, which
  means to apply each of the enclosed properties in parallel.  Particularly
  useful surrounding a set of DEPLOYS applications, to concurrently deploy a
  number of hosts.  We use ``WITH-CURRENT-DIRECTORY`` in various places, so we
  may not be able to do this using threads.  But if we want to do it with lots
  of forking, then practically speaking usage of this combinator will be
  restricted to connection chains which start up remote Lisp images.

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

Project & packaging
-------------------

- Define a semantics for version numbers (probably just like Propellor's),
  start keeping a NEWS file, start actually announcing releases to
  sgo-software-announce.  Take the opportunity to review whole public API for
  good choices of names and sensible separation of responsibilities.
  Increment major version number from zero to one.
