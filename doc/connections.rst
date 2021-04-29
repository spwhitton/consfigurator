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
should restrict its I/O to ``RUN``, ``RUNLINES``, ``READFILE`` and
``WRITEFILE``, functions which access the currently active connection.  This
is in order to permit the arbitrary nesting of connections.  If establishing a
connection really does require more I/O, such as in the case of
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
from sources of prerequisite data.

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

``:CHROOT.FORK``
~~~~~~~~~~~~~~~~

Since forking is typically only possible when it is not the case that multiple
threads are running, it is better to avoid using this connection type as the
first hop, i.e., directly out of the root Lisp (this is not much of a
restriction, since typically the root Lisp is running under a uid which cannot
use the ``chroot(2)`` system call anyway).  More generally, you should avoid
using this connection type within a Lisp image which might try to execute
other deployments in parallel.  Typical usage would be something like::

  (deploy (:sudo :sbcl (:chroot.fork :into "...")) ...)

In some situations you might want to have a connection chain which effectively
uses a connection type like ``:SBCL`` twice in a row, so that the first Lisp
image can execute deployments in parallel while the second forks into the
chroot (typically by having a ``DEPLOYS`` property with connection type
``:SBCL`` as one of the properties applied by a deployment whose connection
chain itself ends with ``:SBCL``).
