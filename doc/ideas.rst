Ideas for future development
============================

Patches welcome.

Properties
----------

- Implementing DEPLOYS and DEPLOYS-THESE is a priority, as it is the first
  step towards implementing things like building disc images.

Connections
-----------

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

- Macro for use in properties which works like Propellor's changesFile.

Project & packaging
-------------------

- Define a semantics for version numbers (probably just like Propellor's),
  start keeping a NEWS file and move from Debian experimental to unstable.

- Add an autopkgtest, marked as superficial, which tries to load the system
  into sbcl and dies if there are any compiler warnings.  Mainly to ensure
  that everything is loaded in the correct order.
