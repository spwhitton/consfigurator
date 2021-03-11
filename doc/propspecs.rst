Property application specifications ("propspecs")
=================================================

Purposes
--------

Property application specifications are a domain-specific language which
enables

- combining properties to make new properties, e.g. with ``DEFPROPLIST``; and

- representing the properties which are to be applied to a host in a form
  which can be serialised using the Lisp printer.

Definitions
-----------

An *atomic property application* ("propapp") is a list satisfying the lambda
list ``(PROPERTY &rest ARGS)`` where ``PROPERTY`` is a symbol naming a
property and ``ARGS`` satisfies that property's lambda list.  A *property
combinator* is a function which takes at least one propapp as one of its
arguments and returns a single propapp, or a macro which takes at least one
form which evaluates to a propapp as one of its arguments, and returns a form
which evaluates to a single propapp.  A *property application specification*
("propspec") is

i. a readably printable Lisp form;
ii. containing propapps combined using property combinators;
iii. containing no free variables except as bound by macro property
     combinators, i.e., such that its evaluation is not affected by any
     dynamic or lexical context;
iv. associated with a list of ASDF systems, and such that
v. evaluating the propspec produces an atomic property application which, when
   those ASDF systems are loaded, applies the propapps in accordance with the
   combinators.

An *unevaluated property application specification* ("unevaluated propspec")
is a list of Lisp forms which can be converted into a property application
specification by

i. replacing each argument in each propapp with the result of evaluating that
   argument in the dynamic and lexical contexts in which the unevaluated
   propspec occurs.
ii. wrapping a single property combinator which takes a variable number of
    arguments around the resulting list of Lisp forms, usually ``SEQUENCE`` or
    ``ALL-OF``.

That is, the arguments to propapps in an unevaluated propspecs are forms which
will produce the arguments to the property, rather than those arguments
themselves.  There is one special case: if the symbol naming the property ends
with the character ``.``, then the propapp is replaced with a new one
according to the following rules (the "dotted propapp rules"):

i. the property to be applied is the property named by the symbol in the same
   package and with the same name as the first element of the propapp, but
   with the trailing period removed from the name;

ii. the first argument is not evaluated if it is a list whose first element is
    a keyword, or a if it is a list of lists where the first element of the
    first list is a keyword; and

iii. the last argument is treated as an embedded unevaluated propspec, and is
     recursively converted into a propspec according to the usual evaluation
     rules, where the surrounding combinator is ``ALL-OF``.

Available combinators
---------------------

``ALL-OF``
~~~~~~~~~~

Function.  Applies each of the propapps passed as arguments without stopping
if any of them signal a failed change.  Semantically, the propapps are ordered
with respect to their ``:HOSTATTRS`` subroutines, but not with respect to
their ``:APPLY`` subroutines.

``SEQUENCE``
~~~~~~~~~~~~

Function.  Applies each of the propapps passed as arguments, stopping and
signalling a failed change if any of the proapps signal a failed change.
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

The elements of unevaluated propspecs are typically arguments to macros, such
that the context of evaluation for forms which produce the arguments to the
propapps is the context in which the call to the containing macro appears.
The single property combinator which will be wrapped around the list of forms
depends on the macro.  ``DEFHOST`` uses ``ALL-OF``, while ``DEFPROPLIST`` uses
``SEQUENCE``.

The dotted propapp rules are intended to make applications of properties like
``DEPLOYS``, ``DEPLOYS-THESE`` and ``CHROOT:DEBOOTSTRAPPED``, which take
property application specifications as arguments, easier to read and write in
the most common cases.  For example, you can write::

  (deploys. (:ssh (:sudo :as "spwhitton@athena.example.com")) athena.example.com
    ((additional-property val1)
     (a-further-property val2)))

instead of::

  (deploys '(:ssh (:sudo :as "spwhitton@athena.example.com")) athena.example.com
    (make-propspec :props `((additional-property ,val1)
                            (a-further-property  ,val2))))

(though note the parentheses around the two propapps must remain, unlike in
``DEPLOY``, ``DEFHOST`` etc.).
