Connections
===========

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

  (deploy (:sudo :debian-sbcl (:chroot.fork :into "...")) ...)

In some situations you might want to have a connection chain which effectively
uses a connection type like ``:DEBIAN-SBCL`` twice in a row, so that the first
Lisp image can execute deployments in parallel while the second forks into the
chroot (typically by having a ``DEPLOYS`` property with connection type
``:DEBIAN-SBCL`` as one of the properties applied by a deployment whose
connection chain itself ends with ``:DEBIAN-SBCL``).
