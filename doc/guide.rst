Concepts
========

Host
----

A machine, container, chroot, or similar.  Has a plist of static informational
*host attributes*, usually including at least a hostname, and an ordered list
of properties it should have, or lack, in the order in which they should be
applied or unapplied (thus properties later in the list implicitly depend on
earlier entries).

Property
--------

Some configuration which a host can have or lack, and which can be added to
a host by running some code, possibly just by applying a series of other
properties.

For example: the presence of some lines in a config file; a package being
installed or absent; the availability of a website.

A property can have a (function returning a) list of host attributes, which
will be added to the attributes plist of any host which has the property in
its list of properties.  These attributes can depend on the arguments to the
property, but should not examine the actual state of the host.

It should also have a check function, which establishes whether the property
is already applied or not.  If this is absent, it is assumed that the property
is always unapplied, i.e., an attempt to apply the property will always be made.

Optionally, it can also have a function which unapplies the property,
permitting creating an inverse of the property.

Connection
----------

A means by which properties can be applied to hosts.  There are two types of
connections: those which interact with the remote host by means of a POSIX
shell, and those which apply properties by executing them in a Lisp process
running on the host.  The keywords ``:posix`` and ``:lisp`` are used to refer
to these types.

``:posix`` connections can pass input to and return output from processes, but
cannot start asynchronous processes for interaction with your Lisp functions.
This is so that ``:posix`` connections can be used to administer hosts for
which shell multiplexing is not possible, such as with serial connections.
For asynchronous interaction, use a ``:lisp`` connection.

The code which establishes a connection is similar to code in ``:posix``
properties: it only performs I/O using functions which access the currently
active connection.  This permits arbitrary nesting of connections.

Deployment
----------

The combination of a connection and a host.  Executing a connection deploys
all of a host's properties to that host by means of the given connection.

A deployment is itself a property.  This means that connections can be
nested: one remote host can be used to deploy others, as a controller.

To deploy single properties, you can use ``host-variant`` to obtain a version
of a host which has all its usual informational attributes, based on its usual
list of properties, but with a different list of properties to be applied.

Root Lisp
---------

The Lisp process you control directly when you execute deployments.  Typically
running on your development laptop/workstation.

Unevaluated property application specification
----------------------------------------------

A property application specification, except in atomic property applications
of the form ``(PROPERTY . ARGS)``, ARGS are expressions to be evaluated to
produce the arguments to pass to PROPERTY, rather than those arguments
themselves.  An unevaluated property application specification can be
converted into a property application specification by replacing each ARG of
ARGS with the result of ``(eval ARG)``.

The main place you will find an unevaluated property application specification
is in a call to DEFHOST.  That macro converts an unevaluated property
application specification into code which will produce the corresponding
property application specification.

Prerequisite data
-----------------

Applying a property may require file contents which should be generated or
extracted at the time of deployment on the machine executing the deployment: a
tarball containing the latest version of the web service to be deployed; a
secret extracted from an encrypted store; a git bundle from localhost which
the target host cannot just clone to itself.

A piece of prerequisite data is identified by two strings.  Typically the
first of these specifies the context in which the data is relevant.  For an
ssh host key, for example, this context would be a hostname.  If it's ``nil``
then the data is valid in any context.  The second of these identifies the
data within its context.  This is often just the filename in which the
prerequisite data will eventually be stored.  It might also be a
human-readable string describing the purpose of the data.

Prerequisite data is versioned.  To replace a secret key, for example, you
change the data and bump the version.  If there is no version bump,
Consfigurator will assume connections can re-use old data; this avoids
uploading the same data over and over again.

Properties declare that they need certain pieces of prerequisite data, and a
deployment of those properties will make an attempt to provide the data.

A :lisp connection gathers all the needed prerequisite data once at the
beginning and copies it to an on-disk cache inside the home directory of the
UID which will run the lisp process on the host which will run it.  A :posix
connection only attempts to obtain prerequisite data when a property's check
function indicates the property is not already applied.

Representing prerequisite data
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A piece of prerequisite data is represented in your configuration by two
functions.  The first will return either a string of the prerequisite data
itself, or a path to a file containing the data.  The second returns the
latest version number of the data -- i.e., the version of the data that the
first function would return if executed.

Consfigurator will call the second function to find out if it needs to call
the first rather than just using its cache.  The first function should return
nil if it can't obtain the prerequisite data on this host, perhaps because it
can't decrypt the store.

Security issues
~~~~~~~~~~~~~~~

Nothing is done to prevent prerequisite data being swapped out, so ensure your
swap is encrypted.

Pitfalls
========

Invoking properties from within properties
------------------------------------------

Properties can programmatically invoke arbitrary properties to be applied in
the context of their current deployment.  But then the informational
attributes of the properties won't be automatically copied to the definition
of the host, so, for example, prerequisite data might be missing.  You will
need to manually add the informational attributes of the property you're
invoking to the informational attributes of the invoking property.

There are other risks in the vicinity: missing informational attributes might
cause other properties to misbehave.  So avoid invoking properties in this way
where you can.  Use property combinators.

When you just want to have a property invoke several others, there are
functions which you can use to define a new property from the list of old
ones, which will set all the informational attributes on the host.

Attempting to work with anonymous properties or connection types
----------------------------------------------------------------

Hosts, property application specifications and deployments are mutable values,
which you can build, pass around and change in your own code.  For example,
deployments can be built and executed programmatically.  However, properties
and connection types should be defined in ``.lisp`` files, loaded into Lisp,
and then *not* created or modified (except by reloading).  In particular, do
not try to define properties and connection types programmatically, or try to
dynamically rebind them.

The reason for this restriction is that some connection types need to invoke
fresh Lisp processes on remote hosts with (equivalents to) the function
objects contained in properties and connections available to be called.  Since
function objects are not serialisable, the only way to do this is to send over
the contents of your ``.lisp`` files and load the same properties and
connection types into the remote Lisp.  By contrast, hosts, property
application specifications and deployments can be serialised and sent over
that way.

If you were to dynamically rebind properties or connection types in the root
Lisp, then connections which do not start remote Lisp processes would use your
new definitions, but connections which start remote Lisp processes would use
the static definitions in your ``.lisp`` files (or lack definitions
altogether).  This would violate the idea in Consfigurator that properties,
including nested deployments, have the same meaning regardless of the
connection types they are used with.

Note that you *can* programmatically determine what arguments will get passed
to properties upon deployment, though each of these arguments needs to be
serialisable, so you can't pass anonymous functions or objects containing
those.
