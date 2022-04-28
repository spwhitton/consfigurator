Connections
===========

Connection chain specifications
-------------------------------

The normalised form is a list of lists, where the car of each inner list is a
keyword symbol identifying a connection type, and the cdr of each inner list
is arguments to that connection, e.g.::

  ((:ssh :foo foo :bar bar) (:sudo :baz baz :quux quux))

There are two notational simplifications permitted when passing connection
chain specifications to properties, functions and macros.  Firstly, for each
inner list which contains only a single keyword identifying a connection type
and no arguments, this list may be replaced with only the keyword identifying
the connection type, e.g.::

  (:ssh (:sudo :baz baz :quux quux))

Secondly, when there is exactly one connection and it takes no arguments, you
may specify just the keyword identifying the connection type, e.g. ``:ssh``.

Note that if there is a single connection but it takes arguments, you will
need two sets of parentheses, i.e.::

  ((:ssh :foo foo :bar bar))

rather than::

  (:ssh :foo foo :bar bar)

which is invalid.

Defining connection types
-------------------------

The code which establishes connections (i.e., implementations of the
``ESTABLISH-CONNECTION`` generic) is like code in ``:posix`` properties -- it
should restrict its I/O to ``RUN``, ``RUNLINES``, ``READ-REMOTE-FILE`` and
``WRITE-REMOTE-FILE``, functions which access the currently active connection.
This is in order to permit the arbitrary nesting of connections.  If
establishing a connection really does require more I/O, such as in the case of
``:CHROOT.FORK`` connections, code can call ``LISP-CONNECTION-P``, and either
signal an error, or fall back to another connection type.

Connection attributes ("connattrs")
-----------------------------------

Information about hosts which cannot be known without looking at the host, or
for other reasons should not be recorded in consfigs, can be stored as
connection attributes, associated with the current connection.  Typically
property combinators set and unset connattrs, and property ``:APPLY`` and
``:UNAPPLY`` subroutines read them.  They can be used to create context for
the application of properties.  Connection attributes are stored in a plist.
Property combinators use the ``WITH-CONNATTRS`` macro to set them, and
properties use ``GET-CONNATTR`` to read them.

Like hostattrs, connection attributes are identified by keywords for connattrs
which are expected to be used in many contexts, and by other symbols for
connattrs which will be used only among a co-operating group of properties and
property combinators.  However, unlike hostattrs, each connattr need not be a
list to which new items are pushed.

By default the list of connattrs is reset when establishing a new connection
within the context of an existing connection.  However, for some connattrs it
makes sense to propagate them along to the new connection.  For example, a
list of connected hardware of a particular type might still be useful in the
context of a connection which chroots, as /dev might still give access to this
hardware.  Implementations of the ``PROPAGATE-CONNATTR`` generic function can
be used to enable propagation where it makes sense.  Methods can copy and
modify connattrs as appropriate; in the chroot example, paths might be updated
so that they are relative to the new filesystem root.

The propagation of connattrs is currently limited to the establishing of
connections within the same Lisp image; i.e., connection types which start up
new Lisp images never propagate any existing connattrs.

Reserved names for connection attributes
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The semantics of connattrs identified by keywords are documented here.

- ``:OPENED-VOLUMES``: instances of ``DISK:OPENED-VOLUME``.  Bound by
  ``DISK:WITH-OPENED-VOLUMES`` property combinator.

Notes on particular connection types
------------------------------------

``:SUDO``
~~~~~~~~~

Passing the ``:AS`` option to this connection means that Consfigurator will
assume a password is required for all commands, and not passing ``:AS`` means
that Consfigurator will assume a password is not required for any commands.
Consfigurator sends your sudo password on stdin, so if the assumption that a
password is required is violated, your sudo password will end up in the stdin
to whatever command is being run using sudo.  There is no facility for
directly passing in a passphrase; you must use ``:AS`` to obtain passwords
from sources of prerequisite data.  The passphrase will be written to a
private temporary file which is deleted when the ``:SUDO`` connection is torn
down.

If any connection types which start up remote Lisp images occur before a
``:SUDO`` entry in your connection chain, ``ESTABLISH-CONNECTION`` will need
to inform the newly-started remote Lisp image of any sudo passwords needed for
establishing the remaining hops.  Depending on how the connection type feeds
instructions to the remote Lisp image, this may involve writing your sudo
password to a file under ``~/.cache`` on the machine which runs the remote
Lisp image.  At least ``:SBCL`` avoids this by sending your password in on
stdin.  Even with ``:SBCL``, if the Lisp image dumps a copy of itself to disk,
e.g. for the purposes of cronjobs, then your sudo password will be contained
in that saved image.  Typically a ``:SUDO`` connection hop is used before hops
which start up remote Lisp images, so these issues will not arise for most
users.

``:SETUID``
~~~~~~~~~~~

As this connection type subclasses FORK-CONNECTION, it shouldn't leak
root-accessible secrets to a process running under the unprivileged UID.
However, when using the :AS connection type, the unprivileged process will
have access to all the hostattrs of the host.  Potentially, something like
ptrace(2) could be used to extract those.  But hostattrs should not normally
contain any secrets, and at least on Linux, the unprivileged process will not
be ptraceable because it was once privileged.

Connections which fork: ``:CHROOT.FORK``, ``:SETUID``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

These connection types cannot be used as the first hop, i.e., directly out of
the root Lisp.  This is because they must call fork(2), and Consfigurator only
makes this system call in contexts in which there shouldn't ever be more than
one thread (excluding Lisp implementation finaliser threads and the like).
The root Lisp is not such a context, because it is often multithreaded due to
the use of SLIME.  This is, however, not much of a restriction, because
typically the root Lisp is running under a UID which cannot use system calls
like chroot(2) and setuid(2) anyway.  Thus, typical usage on localhost would
be something like::

  (deploy (:sudo :sbcl (:chroot.fork :into "...")) ...)

Connections which use setns(2) to enter containers
--------------------------------------------------

When the current connection is a Lisp-type connection, connection types which
enter Linux containers, such as ``:LXC`` and ``:SYSTEMD-MACHINED``, invoke the
setns(2) system call directly.  The implementation of this is the connection
type ``CONSFIGURATOR.CONNECTION.LINUX-NAMESPACE::SETNS``.  The implementation
of the ``POST-FORK`` generic for that connection type is structured similarly
to the nsenter(1) command from util-linux.  This has the advantage that
``CONSFIGURATOR.CONNECTION.LINUX-NAMESPACE::SETNS`` should be reusable for
implementing connection types which enter other kinds of Linux container; the
container runtime-specific code is limited to determining the PID of the
container's leading process.  However, there are some security implications to
this approach.

Firstly, the current implementation does not join the control group of the
container's leading process, and thus the Consfigurator process running inside
the container is not subject to resource limits applied to the container.  It
might be possible for a process in the container to exploit this to escape its
resource limits.

Secondly, we do not attempt to enter the LSM security context of the
container, such as the container's SELinux execution context or AppArmor
profile.  This is because LSM usage is container runtime-specific.  In the
case of unprivileged containers which make use of user namespaces, however,
failing to enter the LSM security context typically does not breach container
security.  For such containers, employment of an LSM serves as an extra layer
of protection against kernel exploits, not as part of the enforcement of the
container's basic security model.
