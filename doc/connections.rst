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

Notes on particular connection types
------------------------------------

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
