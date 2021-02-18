Pitfalls
========

Invoking properties from within properties
------------------------------------------

Properties can programmatically invoke arbitrary properties to be applied in
the context of their current deployment.  However, when this is done the
``:hostattrs`` subroutine of the invoked property will not be called, so, for
example, prerequisite data might be missing.  You will need to add a call to
``PROPATTRS`` in the invoking property's own ``:hostattrs`` subroutine.

There are other risks in the vicinity: missing informational attributes might
cause some other properties to misbehave.  To avoid all this, consider using
``DEFPROPLIST`` to combine properties, rather than having them call each
other.

Attempting to work with anonymous properties or connection types
----------------------------------------------------------------

Hosts, property application specifications and deployments are mutable values,
which you can build, pass around and change in your own code.  For example,
deployments can be built and executed programmatically.  However, properties
and connection types should be defined in ``.lisp`` files, loaded into Lisp,
and then *not* created or modified, except by reloading.  In particular, do
not try to define properties and connection types programmatically, or try to
dynamically rebind or flet-bind them.

The reason for this restriction is that some connection types need to invoke
fresh Lisp processes on remote hosts with (local equivalents to) the function
objects contained in properties and connections available to be called.  Since
function objects are not serialisable, the only way to do this is to send over
the contents of your ``.lisp`` files and load the same properties and
connection types into the remote Lisp.  By contrast, hosts, property
application specifications and deployments can be send over in serialised form.

If you were to dynamically rebind properties or connection types in the root
Lisp, then connections which do not start remote Lisp processes would use your
new definitions, but connections which start remote Lisp processes would use
the static definitions in your ``.lisp`` files (or lack definitions
altogether).  This would violate the idea in Consfigurator that properties,
including nested deployments, have the same meaning regardless of the
connection types they are used with.

Note that you *can* programmatically determine the arguments to pass to
properties upon deployment, though each of these arguments needs to be
serialisable, so you can't pass anonymous functions or objects containing
those.  You can work around the latter restriction by defining a new property
which passes in the desired anonymous function, and then adding the new
property to your property application specification.
