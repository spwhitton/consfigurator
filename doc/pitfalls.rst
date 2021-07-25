Pitfalls and limitations
========================

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

Dumping and reinvoking Lisp
---------------------------

Remote Lisp images can dump executable images of themselves using the
IMAGE-DUMPED property, and some connection types work by dumping and then
immediately reinvoking Lisp.  However, there are some limitations to how these
features can be used that are connected with changing execution context, in the
way that :CHROOT.FORK, :SETUID, and the Linux namespace-entering connections do.

Firstly, for at least some Lisp implementations, the build of Lisp that's
running must be accessible via the filesystem in order for it to be possible
to dump an image.  In the case of SBCL, for example, the very same
/usr/bin/sbcl and /usr/lib/sbcl/sbcl.core need to be accessible.  If
Consfigurator has forked into a chroot then this may not be the case.  So, for
example, when preparing a disk image by applying properties to a chroot, you
can't easily apply a property like CRON:RUNS-CONSFIGURATOR.  See the docstring
for IMAGE-DUMPED for one way to handle this situation.

Secondly, dumped images can fail to start up at all if they cannot reopen all
the shared libraries they had open right before the dump, and if the execution
context has changed, these files might not be readable anymore.  For example,
if Consfigurator has forked into a chroot and then dumped an image, libacl1.so
might not be present at all, or -- what is more likely in the case of that
particular library -- only accessible via a different path.  Additionally, the
use of the CFFI groveller by Consfigurator and its dependency Osicat means
that the reinvoked image will try to load shared libraries out of the
~/.cache/common-lisp belonging to the user who originally started up the
remote Lisp image.  For example, if :SETUID has been used to switch from root
to an unprivileged user, and then an image is dumped, the unprivileged user
won't be able to execute that image.  This is because the unprivileged user
cannot typically read files under /root/.cache/common-lisp.

This second issue could be partly mitigated using CFFI's ``STATIC-PROGRAM-OP``
ASDF operation, as described in the "Static Linking" section of the CFFI
manual.  This is not currently implemented, for several reasons.  Firstly, it
is less portable than the current CFFI features we use, and does not actually
solve the problem of inaccessible system libraries like libacl1.so, only the
problem of files under ~/.cache/common-lisp.  Secondly, there are unsolved
difficulties integrating it with some of Consfigurator's standard usage
patterns, such as calls to TRY-REGISTER-DATA-SOURCE which appear directly in
consfigs (STATIC-PROGRAM-OP tries to load up consfigs in another Lisp image
without a way for us to bind *NO-DATA-SOURCES*).  Thirdly, [CFFI currently
tries to reopen all shared libraries upon reinvocation regardless of the use
of STATIC-PROGRAM-OP](https://github.com/cffi/cffi/pull/163).  Finally, many
builds of SBCL (including Debian's at the time of writing) can't get all the
way through STATIC-PROGRAM-OP without hacks like manually setting the
SBCL_HOME environment variable before starting the attempt.
