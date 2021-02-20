Properties
==========

Property subroutines
--------------------

A property is composed of four subroutines, which all take the same
arguments.  At least one of ``:hostattrs`` or ``:apply`` must be present.

``:hostattrs`` subroutines
~~~~~~~~~~~~~~~~~~~~~~~~~~

Executed in the root Lisp to (i) add static informational attributes of hosts
to which this property is applied or is to be applied; and (ii) check that
applying this property makes sense -- e.g. that we're not trying to install a
package using apt(1) on a FreeBSD host.

Can retrieve existing static informational attributes using ``GET-HOSTATTRS``,
or things which wrap ``GET-HOSTATTRS``, such as ``GET-HOSTNAME``.  Should
signal the condition ``INCOMPATIBLE-PROPERTY`` if existing static
informational attributes indicate that the property should not be applied to
this host.  Can use ``PUSH-HOSTATTRS`` and ``REQUIRE-DATA`` to add new entries
to the host's static information atributes.

Other than as described in the previous paragraph, should be a pure function.
In particular, should not examine the actual state of the host.  Essentially a
conversion of the arguments to the property to appropriate static
informational attributes.

``:check`` subroutines
~~~~~~~~~~~~~~~~~~~~~~

Determine whether or not the property is already applied to the host and
return a generalised boolean indicating such.  Whether or not the ``:apply``
and ``:unapply`` subroutines get called depends on this return value.  If
absent, it is always assumed the property is applied, i.e., an attempt to
apply the property will always be made.

``:apply`` and ``:unapply`` subroutines
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Apply or unapply the property.  Should return ``:nochange`` if the property
was already applied; any other return value is interpreted as meaning that the
property was not (fully) applied before we ran, but now it is.  (If the
``:check`` function indicated that neither ``:apply`` nor ``:unapply`` should
be run, then this is equivalent to those subroutines returning ``:nochange``.)

The point of having both these return value semantics and the ``:check``
subroutine is that a property might only be able to check whether it made a
change after trying to apply itself -- it might check whether running a
command actually made a change to a particular file, for example.

Errors in attempting to apply a property are indicated by signalling a
``FAILED-CHANGE`` error condition.

``:posix`` vs. ``:lisp`` properties
-----------------------------------

``:posix`` properties should not make any assumptions about what localhost is
-- they may be running in the root Lisp, but they might be running in a Lisp
image running on an intermediary host, or even on the host to be configured.
They should perform I/O only by calling ``RUN``, ``RUNLINES``, ``READFILE``,
``WRITEFILE``, requesting prerequisite data, and applying or unapplying other
``:posix`` properties.  Otherwise, they should be pure functions.

``:lisp`` properties, by contrast, may (and should) assume that they are
running in a Lisp image on the host to which they are to be applied, so they
can perform arbitrary I/O in that context.  They can also make use of ``RUN``,
``RUNLINES``, ``READFILE`` and ``WRITEFILE`` if desired.
