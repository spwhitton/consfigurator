Ideas for future development
============================

Patches welcome.

Properties
----------

- Implementing DEPLOYS and DEPLOYS-THESE is a priority, as it is the first
  step towards implementing things like building disc images.

Connections
-----------

- POSIX-CONNECTION which runs commands in a chroot, and a corresponding
  LISP-CONNECTION which forks into the chroot.  The latter will make a system
  call so it will be an implementation of ESTABLISH-CONNECTION which does not
  behave like a :POSIX property.  So I think we actually want a generic for
  each connection type keyword symbol, which returns whether establishing a
  connection of that type requires the most recent hop to be POSIX- or LISP-.
  Then DEPLOY* can call that and error out if establishing the next hop
  requires LISP- but we only have POSIX-.

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

- Macro for use in DEFPROP which works like Propellor's changesFile.  Will
  probably output ``(:check ...)`` expression and then substitute user's code
  into a ``(:apply ...)`` expression.  Use this or variants thereof in most of
  the entries in ``PROPERTY.FILE``.

- HOSTDEPLOY and HOSTDEPLOY-THESE functions which are like DEPLOY and
  DEPLOY-THESE but take the CONNECTION argument from definitions established
  by DEFHOSTDEPLOY.

Project & packaging
-------------------

- Define a semantics for version numbers (probably just like Propellor's),
  start keeping a NEWS file and move from Debian experimental to unstable.

- Add an autopkgtest, marked as superficial, which tries to load the system
  into sbcl and dies if there are any compiler warnings.  Mainly to ensure
  that everything is loaded in the correct order.
