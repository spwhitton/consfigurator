Introduction
============

Consfigurator is a system for declarative configuration management using
Common Lisp.  You can use it to configure hosts as root, deploy services as
unprivileged users, and build disc images.

Quick start / introduction
==========================

1. Create a new directory ``consfig`` somewhere where ASDF will pick it up,
   such as ``~/common-lisp/consfig``.

2. Define a Lisp system which represents your configuration.

    ~/common-lisp/consfig/com.example.consfig::

        (asdf:defsystem :com.example.consfig
          :serial t
          :depends-on (#:consfigurator)
          :components ((:file "package")
                       (:file "consfig")))

    ~/common-lisp/consfig/package.lisp::

        (in-package :cl-user)

        (defpackage :com.example.consfig
          (:use #:cl #:consfigurator)
          (:local-nicknames (#:file #:consfigurator.property.file)))

3. Define some hosts and connections.

    ~/common-lisp/consfig/consfig.lisp::

        (in-package :com.example.consfig)

        (setconsfig :com.example.consfig)

        (defhost athena.example.com
          "Web and file server."
          (file:contains-lines "/etc/default/locale" '("LANG=en_GB.UTF-8")))

        (defhostdeploy :ssh athena.example.com)

4. Get a Lisp REPL started up and evaluate ``(asdf:require-system
   "com.example.consfig")``.

5. Now you should be able to use configure athena by evaulating
   ``(athena.example.com)``.  You can use the DEPLOY function to try out
   configuring athena using a different connection type than defined here.

Portability and stability
=========================

- **Consfigurator is still stabilising and so there may be breaking changes.**

- No attempt is made to support Common Lisp implementations other than SBCL,
  though portability patches are welcome.

- No attempt is made to support running on Windows -- we often eschew Common
  Lisp pathnames in favour of simple strings with forward slashes as directory
  separators.

Credits
=======

Many of the good ideas here come straight from Joey Hess's Propellor_.  I'm
working on Consfigurator because I think Propellor is great, but wanted to add
Consfigurator's ``:posix`` connections and arbitrary connection nesting --
Propellor supports something equivalent to a single ``:lisp`` connection --
and I wanted to implement that in Lisp.  Also, after five years of using and
extending Propellor, I've come to disagree with Joey about whether Haskell's
type system helps or hinders using and extending Propellor.

.. Propellor_ https://propellor.branchable.com/
