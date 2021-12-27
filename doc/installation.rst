.. _Installation:

Installation
============

Debian and Debian derivatives
-----------------------------

The most recent tagged release of Consfigurator is usually included in `Debian
unstable`_, and the .deb there should work fine on Debian stable and testing,
and derivatives like Ubuntu.  After adding an apt source for unstable if
necessary, ``apt-get install cl-consfigurator/unstable``.

.. _Debian unstable: https://www.debian.org/doc/manuals/debian-faq/choosing.en.html

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
