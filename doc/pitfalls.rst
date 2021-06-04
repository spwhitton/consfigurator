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
fresh Lisp images on remote hosts with (local equivalents to) the function
objects contained in properties and connections available to be called.  Since
function objects are not serialisable, the only way to do this is to send over
the contents of your ``.lisp`` files and load the same properties and
connection types into the remote Lisp.  By contrast, hosts, property
application specifications and deployments can be send over in serialised form.

If you were to dynamically rebind properties or connection types in the root
Lisp, then connections which do not start remote Lisp images would use your
new definitions, but connections which start remote Lisp images would use
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

Code-walking limitations
------------------------

The preprocessing of propspecs, and the conversion of unevaluated propspecs
into propspecs, both require code walking.  Consfigurator's implementation of
this is in the function ``MAP-PROPSPEC-PROPAPPS``.  However, due to
limitations in the Common Lisp standard, it is not possible to implement the
work of that function in a way which is both always correct and fully
portable.  I have not found a general purpose code walker which hooks into
implementation-specific functionality and that is currently maintained, and so
at present we use a best-effort portable code walker, Agnostic Lizard.

This will almost always generate the correct expansions, but if you have
particularly advanced macro property combinators then it is possible that
``MAP-PROPSPEC-PROPAPPS`` will return incorrectly expanded forms.  For full
details see Michael Raskin.  2017.  "Writing a best-effort portable code
walker in Common Lisp."  In *Proceedings of 10th European Lisp Symposium*,
Vrije Universiteit Brussel, Belgium, April 2017 (ELS2017).  DOI:
10.5281/zenodo.3254669.

It is possible to implement the work of ``MAP-PROPSPEC-PROPAPPS`` in terms of
``MACROEXPAND-ALL``, whose semantics are conventionally well-understood and
for which fully correct implementations are available in most implementations
of Common Lisp (the trivial-macroexpand-all library can be used to get at
these).  However, note that we cannot just call ``MACROEXPAND-ALL`` on
propspecs because unquoted lists appearing as arguments to properties in
atomic property applications will look like invalid function calls to the code
walker.  Avoiding this would seem to require wrapping the propspec in one
macrolet for each known property, and this makes ``MACROEXPAND-ALL`` too slow,
even if the macrolet forms are precomputed.
