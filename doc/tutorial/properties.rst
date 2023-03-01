Defining new properties
=======================

Defining new properties is like defining new functions: ``DEFPROP``,
``DEFPROPLIST`` and ``DEFPROPSPEC`` are more like ``DEFUN`` than anything
else.  Here is a guide to these three macros; in particular, why you might
need to move from the basic ``DEFPROPLIST`` to either ``DEFPROPSPEC`` or
``DEFPROP``.

.. include:: conventions.rst

``DEFPROPLIST`` and ``DEFPROPSPEC``
-----------------------------------

These macros allow you to define properties by combining existing properties.
Most user properties in consfigs should be defineable using one of these: you
should not need to resort to ``DEFPROP``.  And indeed, most new properties to
be added to Consfigurator itself should not need to use ``DEFPROP`` either.

``DEFPROPLIST``
~~~~~~~~~~~~~~~

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
~~~~~~~~~~~~~~~

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
-------

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
