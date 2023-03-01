Properties
==========

Defining new properties
-----------------------

Defining new properties is like defining new functions: ``DEFPROP``,
``DEFPROPLIST`` and ``DEFPROPSPEC`` are more like ``DEFUN`` than anything
else.  Here is a guide to these three macros; in particular, why you might
need to move from the basic ``DEFPROPLIST`` to either ``DEFPROPSPEC`` or
``DEFPROP``.

``DEFPROPLIST`` and ``DEFPROPSPEC``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

These macros allow you to define properties by combining existing properties.
Most user properties in consfigs should be defineable using one of these: you
should not need to resort to ``DEFPROP``.  And indeed, most new properties to
be added to Consfigurator itself should not need to use ``DEFPROP`` either.

``DEFPROPLIST``
^^^^^^^^^^^^^^^

``DEFPROPLIST`` allows you to define properties very similarly to how you
define hosts with ``DEFHOST``.  You simply supply an unevaluated property
application specification.  Compare:

::

  (defproplist setup-test-pure-wayland ()
    "Set up system for testing programs' support for running Wayland-native."
    (apt:installed "sway")
    (apt:removed "xwayland")
    (systemd:disabled "lightdm"))

  (defhost laptop.example.com ()
    (os:debian-stable "bullseye" :amd64)
    #| ... |#
    (setup-test-pure-wayland)
    ;; Previously we had these; now factored out:
    ;; (apt:installed "sway")
    ;; (apt:removed "xwayland")
    ;; (systemd:disabled "lightdm")
    #| ... |#)

You can use parameters just like with plain functions:

::

  (defproplist setup-test-pure-wayland (compositor)
    "Set up system for testing programs' support for running Wayland-native."
    (apt:installed compositor)
    (apt:removed "xwayland")
    (systemd:disabled "lightdm"))

  (defhost laptop.example.com ()
    (os:debian-stable "bullseye" :amd64)
    #| ... |#
    (setup-test-pure-wayland "sway")
    #| ... #|)

To compute intermediate values, you can use ``&aux`` parameters.  Be aware
that code for ``&optional`` and ``&key`` default values, and for ``&aux``
parameters, may be executed more than once per application of the property, so
it should usually be side effect-free.

``DEFPROPSPEC``
^^^^^^^^^^^^^^^

Unevaluated property application specifications are not always as expressive
as is required.  For example, what if, in our example, we want to allow the
user to supply a list of packages to install, of arbitrary length?  For this
we need ``DEFPROPSPEC``.  The body of this macro is ordinary Lisp code which
should return a property application specification.  In most cases all you
need is a single backquote expression:

::

  (defpropspec setup-test-pure-wayland :posix (&rest compositor-packages)
    "Set up system for testing programs' support for running Wayland-native."
    `(eseqprops
      (apt:installed ,@compositor-packages)
      (apt:removed "xwayland")
      (systemd:disabled "lightdm")))

  (defhost laptop.example.com ()
    (os:debian-stable "bullseye" :amd64)
    #| ... |#
    (setup-test-pure-wayland "sway" "swayidle" "swaylock")
    #| ... #|)

Use this basic shape, with ``ESEQPROPS``, if you want ``DEFPROPLIST`` with
just a little more expressive power -- ``DEFPROPLIST`` has an implicit
``ESEQPROPS``.

If you want to include elements of the property application specification
conditionally, you will need ``DEFPROPSPEC``.  For example, perhaps disabling
lightdm is not appropriate on all hosts to which we want to apply this
property, because not all of them have it installed.  So we might use::

  (defpropspec setup-test-pure-wayland :posix (lightdmp &rest compositor-packages)
    "Set up system for testing programs' support for running Wayland-native."
    `(eseqprops
      (apt:installed ,@compositor-packages)
      (apt:removed "xwayland")
      ,@(and lightdmp '((systemd:disabled "lightdm")))))

  (defhost laptop.example.com ()
    (os:debian-stable "bullseye" :amd64)
    #| ... |#
    (setup-test-pure-wayland t "sway" "swayidle" "swaylock")
    #| ... #|)

The expression ``,@(and x '((y)))`` (or ``,@(and x `((,y)))``) is a Lisp
idiom for conditional inclusion of sublists of backquoted lists.

One disadvantage of moving to ``DEFPROPSPEC`` (aside from just being less
declarative) is that you can't use unevaluated property application
specification-specific features, such as dotted propapp notation, directly
within backquote expressions.  This won't work:

::

  (defpropspec setup-... :lisp (...)
    `(eseqprops
      #| ... |#
      ;; won't work
      (chroot:os-bootstrapped. nil "/srv/chroot/unstable-amd64"
        (os:debian-unstable :amd64)
	(apt:installed "build-essential"))
      #| ... |#))

However, use of the ``PROPAPP`` macro makes it possible to temporarily switch
back to something more like ``DEFPROPLIST``:

::

  (defpropspec setup-... :lisp (...)
    `(eseqprops
      #| ... |#
      ,(propapp (chroot:os-bootstrapped. nil "/srv/chroot/unstable-amd64"
                  (os:debian-unstable :amd64)
	          (apt:installed "build-essential")))
      #| ... |#))

In all these examples, the body of the ``DEFPROPSPEC`` has been a single form.
Sometimes you will need to wrap binding forms around this, or precede it with
other forms, to compute the propspec expression.  In some cases you might not
use backquote at all; see ``INSTALLER:BOOTLOADER-BINARIES-INSTALLED`` for an
example.

Note that arguments to property applications within backquotes are not
evaluated.  Whereas you might use::

  (file:contains-lines "/etc/foo" '("bar" "baz"))

within ``DEFPROPLIST``, within backquotes within ``DEFPROPSPEC`` you would
need::

  (file:contains-lines "/etc/foo" ("bar" "baz"))

DEFPROP
~~~~~~~

Returning to our ``SETUP-TEST-PURE-WAYLAND`` example, it's not great that the
user has to supply a parameter specifying whether or not lightdm needs to be
disabled.  We should just check whether lightdm is installed, and disable it
only if it's there (some sort of check is necessary because
``SYSTEMD:DISABLED`` will fail if there is no service to disable).

``DEFPROP`` is more fundamental than both ``DEFPROPLIST`` and ``DEFPROPSPEC``:
it allows you to supply arbitrary code for each of the property's subroutines
(see :ref:`property-subroutines`, below).  In our example, the crucial
difference is that ``DEFPROPLIST`` and ``DEFPROPSPEC`` permit you to run code
only before any connection to the host is established, which, of course, is
too early to check whether lightdm is installed.  We need to run the check at
``:APPLY`` time:

::

  (defprop %no-lightdm :posix ()
    (:hostattrs (os:required 'os:debianlike))
    (:apply (if (apt:all-installed-p "lightdm")
                (systemd:disabled "lightdm"))
                :no-change))

  (defpropspec setup-test-pure-wayland :posix (&rest compositor-packages)
    "Set up system for testing programs' support for running Wayland-native."
    `(eseqprops
      (apt:installed ,@compositor-packages)
      (apt:removed "xwayland")
      (%no-lightdm)))

Here, we can call the ordinary function ``APT:ALL-INSTALLED-P`` to examine the
actual state of the host.  We have had to introduce two complexities to
account for several implicit features of ``DEFPROPLIST`` and ``DEFPROPSPEC``.
Firstly, we have to specify that the property is only applicable to
Debian-like hosts (in this case we could get away with not doing that because
we apply ``%NO-LIGHTDM`` only within a ``DEFPROPSPEC`` that has other
properties which are applicable only to Debian-like hosts, but that's highly
contingent).  And secondly, we have to take care to return ``:NO-CHANGE`` in
the case that lightdm is not installed.  Both of these things are taken care
of for us with ``DEFPROPLIST`` and ``DEFPROPSPEC``.  Nevertheless, if you need
to examine the actual state of the host, only ``DEFPROP`` will do.

Finally, note how we keep the rest of ``SETUP-TEST-PURE-WAYLAND`` in a
``DEFPROPSPEC``, only dropping down to ``DEFPROP`` for the part that requires
it.  This is good practice.

Names
-----

The names of properties may not end in the character ``.``, because that has a
special meaning in unevaluated property application specifications.

Properties with ``:APPLY`` subroutines occupy the function cells of symbols,
so except in the case of properties with no ``:APPLY`` subroutine, do not try
to define an ordinary function with the same name as a property.

Working directories
-------------------

Except where specified otherwise in property docstrings, relative paths are
relative to the remote home directory.  ``:LISP`` properties may assume they
will be executed in the remote home directory, and ``:POSIX`` properties may
assume that commands will be executed in the remote home directory, and that
relative paths passed to ``READ-REMOTE-FILE`` and ``WRITE-REMOTE-FILE`` are
relative to the remote home directory.  Use ``WITH-REMOTE-CURRENT-DIRECTORY``
to change the remote working directory in a way which ensures it will get
changed back.

.. _property-subroutines:

Property subroutines
--------------------

A property is composed of up to five subroutines, which all have the same
lambda list (take the same arguments).  At least one of ``:hostattrs``,
``:apply`` or ``:unapply`` must be present.

``:desc`` subroutines
~~~~~~~~~~~~~~~~~~~~~

Pure function of the property's arguments which returns a description of
applying the property, to be used in stdout by deployments to inform the user
what work is being done.

``:preprocess`` subroutines
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Pure function executed to modify the arguments that will be passed to the
other subroutines; should return a fresh list of the new arguments.  This
subroutine is called on each atomic property application within a property
application specification before the effects of property combinators have been
applied.  That is, it is effectively executed on atomic property applications
in isolation from the property application specifications in which they occur.

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
absent, it is always assumed the property is unapplied, i.e., an attempt to
apply the property will always be made.

``:apply`` and ``:unapply`` subroutines
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Apply or unapply the property.  Should return ``:no-change`` if the property
was already applied; any other return value is interpreted as meaning that the
property was not (fully) applied before we ran, but now it is.  (If the
``:check`` function indicated that neither ``:apply`` nor ``:unapply`` should
be run, then this is equivalent to those subroutines returning ``:no-change``.)

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
They should perform I/O only by calling ``RUN``, ``RUNLINES``,
``READ-REMOTE-FILE``, ``WRITE-REMOTE-FILE``, requesting prerequisite data, and
applying or unapplying other ``:posix`` properties.  Otherwise, they should be
pure functions.

``:lisp`` properties, by contrast, may (and should) assume that they are
running in a Lisp image on the host to which they are to be applied, so they
can perform arbitrary I/O in that context.  They can also make use of ``RUN``,
``RUNLINES``, ``READ-REMOTE-FILE`` and ``WRITE-REMOTE-FILE`` if desired.

``:posix`` properties are characterised by the limited set of ways in which
they perform I/O, not by the use of only facilities defined in the Single UNIX
Specification.  Nevertheless, if a ``:posix`` property or function intended to
be called by ``:posix`` properties uses non-POSIX facilities, but it is not
obvious given the stated purpose of the property that it will do this, it is
good to mention the use of non-POSIX facilities in the docstring.  For
examples of this, see ``USER:HAS-LOGIN-SHELL`` and ``USER:PASSWD-FIELD``.
