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
