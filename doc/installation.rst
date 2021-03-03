.. _Installation:

Installation
============

Debian and Debian derivatives
-----------------------------

The most recent tagged release of Consfigurator is included in `Debian
experimental`_, and the .deb there should work fine on Debian stable, testing
and unstable, and derivatives like Ubuntu.  After enabling the repository,
``apt-get install cl-consfigurator/experimental``.

.. _Debian experimental: https://wiki.debian.org/DebianExperimental

Quicklisp
---------

The most recent tagged release of Consfigurator is included in the
`Quicklisp`_ service: ``(ql:quickload "consfigurator")``.

.. _Quicklisp: https://www.quicklisp.org/

From git
--------

If you would like to follow development more closely, you can::

    % git clone https://git.spwhitton.name/consfigurator ~/.local/share/common-lisp/source/consfigurator

and ASDF should pick it up.
