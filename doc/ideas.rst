Ideas for future development
============================

Patches welcome.

Properties
----------

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
  DEPLOY-THESE but take the CONNECTION argument from the :DEPLOY argument to
  DEFHOST.

- A CONCURRENTLY combinator for property application specifications, which
  means to apply each of the enclosed properties in parallel.  Particularly
  useful surrounding a set of DEPLOYS applications, to concurrently deploy a
  number of hosts.

- It might be useful to have a restart for the case where an attempt is made
  to apply a list of properties containing some ``:LISP`` properties with a
  POSIX-type connection which applies properties up to but not including the
  first ``:LISP`` property in the sequence, to get as much work as possible
  done without violating any dependency relationships (``SEQPROPS`` already
  handles wanting to apply all of the ``:POSIX`` properties in the sequence).
  But maybe this is unnecessarily complex -- wouldn't it be better to just
  fail and fix your deployment definitions?

- It's not clear that the current implementation of DEFPROPLIST can support
  everything we might want to do with it.  An alternative approach to try out
  would be for a call to DEFPROPLIST to expand into a macro definition, where
  the new macro expands into the properties to be combined with DEFPROPLIST,
  surrounded by ESEQPROPS.  Calls to this new macro can then just be added to
  propspecs by users.  So in effect we would have a macro which can be used in
  place of a propapp.

Project & packaging
-------------------

- Define a semantics for version numbers (probably just like Propellor's),
  start keeping a NEWS file, move from Debian experimental to unstable,
  start actually announcing releases to sgo-software-announce.

- Provide a ``:consfigurator`` named readtable which enables both CL-INTERPOL
  and CL-HEREDOC syntax.  Use it in all source files, and for users in their
  consfigs.
