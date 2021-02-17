Property subroutines
~~~~~~~~~~~~~~~~~~~~

``:hostattrs`` subroutines
==========================

When this subroutine is executed, consfigurator:*hostattrs* will be bound to
the plist of static information attributes of the host to which the property
has been applied or is to be applied.  This subroutine typically pushes new
entries to this list, but it might also modify existing entries (e.g. by
pushing new entries to a sublist).

Should be a pure function aside from looking at consfigurator:*hostattrs*.
Essentially just a conversion of the arguments to the property to
informational attributes.

``:check`` subroutines
======================

Determine whether or not the property is already applied to the host and
return a generalised boolean indicating such.  Whether or not the ``:apply``
and ``:unapply`` subroutines get called depends on this return value.

``:apply`` and ``:unapply`` subroutines
=======================================

The return value or values is up to you, but a few keywords as the first
return value are treated specially.

- ``:madechange`` -- the property was not (fully) applied before we ran, but
  now it is.

- ``:nochange`` -- the property was already applied

If neither of these values are returned but one of the ``:apply`` and
``:unapply`` subroutines was executed, then it is assumed that the property
did make a change.  If the ``:check`` function indicated that neither of these
subroutines should be run, it is assumed that the property did not make a
change.

The point of having both these return value semantics and the ``:check``
subroutine is that a property might only be able to check whether it made a
change after trying to apply itself -- it might check whether running a
command actually made a change to a particular file, for example.

Errors in attempting to apply a property are indicated by signalling a
``failed-change`` condition.

``:posix`` vs. ``:lisp`` properties
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

``:posix`` properties should not make any assumptions about what localhost is
-- they may be running in the root Lisp, but they might be running in a Lisp
process running on an intermediary host, or even on the host to be configured.
They should perform I/O only by calling ``run``, ``readfile``, ``writefile``,
requesting prerequisite data, and applying or unapplying other ``:posix``
properties.  Otherwise, they should be pure functions.

In this respect, the code which establishes connections (i.e., implementations
of the ``establish-connection`` generic function) is like a ``:posix``
property -- it should restrict its I/O to ``run``, ``readfile`` and
``writefile`` to permit the arbitrary nesting of connections.

``:lisp`` properties, by contrast, may assume that they are running in a Lisp
process on the host to which they are to be applied, so they can perform
arbitrary I/O in that context.  They can also make use of ``run``,
``readfile`` and ``writefile`` if desired.
