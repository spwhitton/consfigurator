Ideas for future development
============================

Patches welcome.

Properties
----------

- Implementing DEPLOYS and DEPLOYS-THESE is a priority, as it is the first
  step towards implementing things like building disc images.

Connections
-----------

- :DEBIAN-SBCL could (fork and) SAVE-LISP-AND-DIE.  That way, we have
  something that a cronjob can call to re-run the deployment to ensure that
  all properties remain applied.  Need to think about how the property which
  sets up the cronjob will be specified in consfigs -- does it make sense to
  allow passing in arbitrary deployments, or do we only allow re-running
  exactly the same thing?  If the former, the saved image will need to take
  some sort of command line input telling it what arguments to pass to
  DEPLOY*.

- Basic infrastructure for connections which work with just input and output
  streams connected to an interactive POSIX sh somewhere, like TRAMP, and
  probably using ``base64 -d`` for WRITEFILE.  Probably the basic connection
  type will take a command to start up the shell as a keyword argument, and
  then we can have more specific connection types which take other arguments
  and construct the full command.

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

- A CONCURRENTLY combinator for property application specifications, which
  means to apply each of the enclosed properties in parallel.  Particularly
  useful surrounding a set of DEPLOYS applications, to concurrently deploy a
  number of hosts.

Project & packaging
-------------------

- Define a semantics for version numbers (probably just like Propellor's),
  start keeping a NEWS file and move from Debian experimental to unstable.
