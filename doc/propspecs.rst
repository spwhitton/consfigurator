Property application specifications ("propspecs")
=================================================

If you understand the difference between propspecs and unevaluated propspecs
well enough to be able to read and understand most of the uses of
``DEFPROPLIST`` in Consfigurator's own source, then you can probably get away
without working through this section of the manual.  You can build your own
propspecs by taking uses of ``DEFPROPLIST`` in Consfigurator's own source as
models.  What follows is mainly for those who want to implement new property
combinators, though normal usage of Consfigurator should not require doing
that.

Purposes
--------

Property application specifications are a domain-specific language which
enables

1. combining properties to make new properties, e.g. with ``DEFPROPLIST``; and

2. representing the properties which are to be applied to a host in a form
   which can be serialised using the Lisp printer.

Definitions
-----------

An *atomic property application* ("propapp") is a list satisfying the lambda
list ``(PROPERTY &rest ARGS)`` where ``PROPERTY`` is a symbol naming a
property and ``ARGS`` satisfies that property's lambda list, or the empty
list, which means a no-op property.  A *property combinator* is a function
which takes at least one propapp as one of its arguments and returns a single
propapp, or a macro which takes at least propapp as one of its arguments and
returns an expression which yields a single propapp (in the return values of
property combinators, often ``PROPERTY`` will be a gensym).  A *property
application specification* is

i. a readably printable Lisp expression, **P**, known as a *property
   application specification expression*, which contains

   a. propapps combined using property combinators; but

   b. no free variables except as bound by macro property combinators, i.e.,
      such that the evaluation of **P** would not be affected by any dynamic
      or lexical context; where

   c. evaluating **P** means an operation equivalent to quoting each propapp
      occurring in **P**, and then passing the whole thing as the only
      argument to ``EVAL``; and

ii. a list of ASDF systems **S**; such that

iii. evaluating **P** yields a propapp ``(PROPERTY &rest ARGS)``; such that

iv. when each of **S** are loaded, calling ``PROPERTY`` with ``ARGS`` will
    apply each of the propapps occurring in **P** in accordance with the
    combinators occuring in **P**.

A property application specification should not contain any binding forms
except as will be eliminated after macroexpansion.

We do not always cleanly distinguish between property application
specifications and property application specification expressions, and the
abbreviation "propspec" refers to both.  It is usually clear from the context
which is meant.  Within Lisp they are easy to distinguish because property
application specifications are CLOS objects, whereas property application
specification expressions are just conses.

An *unevaluated property application specification expression* ("unevaluated
propspec") is a list of Lisp forms which can be converted into a property
application specification expression by

i. replacing each argument in each propapp with the result of evaluating that
   argument in the dynamic and lexical contexts in which the unevaluated
   propspec occurs; and

ii. wrapping a single property combinator which takes a variable number of
    arguments around the resulting list of Lisp forms, usually ``SEQPROPS`` or
    ``ESEQPROPS``.

That is, the arguments to propapps in unevaluated propspecs are forms which
will produce the arguments to the properties, rather than those arguments
themselves.  There is one special case: if the symbol naming the property ends
with the character ``.``, then the propapp is replaced with a new one
according to the following rules (the "dotted propapp rules"):

i. the property to be applied is the property named by the symbol in the same
   package and with the same name as the first element of the propapp, but
   with the trailing period removed from the name;

ii. the first argument is not evaluated if it is a list whose first element is
    a keyword, or a if it is a list of lists where the first element of the
    first list is a keyword; and

iii. the last required or optional argument to the property to be applied is
     converted to a ``&rest`` parameter and its elements are treated as an
     embedded unevaluated propspec, which is recursively converted into a
     propspec according to the usual evaluation rules, where the surrounding
     combinator is ``ESEQPROPS``.

The dotted propapp rules are implemented by ``DEFPROP`` and ``DEFPROPLIST``,
which define a macro with the dotted name which performs the replacement.

Available combinators
---------------------

``SEQPROPS``
~~~~~~~~~~~~~

Function.  Applies each of the propapps passed as arguments without stopping
if any of them signal a failed change.  Semantically, the propapps are ordered
with respect to their ``:HOSTATTRS`` subroutines, but not with respect to
their ``:APPLY`` subroutines.

``ESEQPROPS``
~~~~~~~~~~~~~

Function.  Applies each of the propapps passed as arguments, stopping and
signalling a failed change if any of the propapps signal a failed change.
Semantically, each propapp implicitly depends upon the preceding propapps.

``UNAPPLY``
~~~~~~~~~~~

Function.  Unapplies a single propapp.

``ON-CHANGE``
~~~~~~~~~~~~~

Macro.  Applies properties when attempting to apply the first did not return
``:NO-CHANGE``.

Remarks
-------

The conversion of an unevaluated propspec into a propspec must resolve any
free variable references, except where those will be resolved by macro
property combinators.

The ``PROPS`` macro converts an unevaluated propspec into a propspec.

The evaluation of arguments to propapps in unevaluated propspecs appearing in
calls to ``DEFHOST``, ``DEPLOYS.`` and ``DEPLOYS-THESE.`` cannot retrieve
hostattrs, because these propspecs will be evaluated as part of the initial
definitions of hosts, before they have any hostattrs.  By contrast, the
unevaluated propspecs in calls to ``DEFPROPLIST``, ``DEPLOY``, and
``DEPLOY-THESE``, and the code which produces propspecs in ``DEFPROPSPEC``,
may retrieve hostattrs set by other properties, because that code is run in
the context of a host which has already been defined.  You cannot retrieve
hostattrs set by properties in the propspec resulting from evaluating the
evaluated propspec, however, since that propspec has not yet been applied to
the host.  New hostattrs should not be pushed outside of the definitions of
``:HOSTATTRS`` subroutines.

The elements of unevaluated propspecs are typically arguments to macros, such
that the context of evaluation for forms which produce the arguments to the
propapps is the context in which the call to the containing macro appears.
The single property combinator which will be wrapped around the list of forms
depends on the macro.  ``DEFHOST`` uses ``SEQPROPS``, while ``DEFPROPLIST``
uses ``ESEQPROPS``.

The dotted propapp rules are intended to make applications of properties like
``DEPLOYS``, ``DEPLOYS-THESE`` and ``CHROOT:DEBOOTSTRAPPED``, which take
property application specifications as arguments, easier to read and write in
the most common cases.  For example, you can write::

  (deploys. (:ssh (:sudo :as "spwhitton@athena.example.com")) athena.example.com
    (a-further-property  val1)
    (additional-property val2))

instead of::

  (deploys '(:ssh (:sudo :as "spwhitton@athena.example.com")) athena.example.com
    (make-propspec :props `((a-further-property  ,val1)
                            (additional-property ,val2))))

