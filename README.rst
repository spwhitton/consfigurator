Introduction
============

Consfigurator is a system for declarative configuration management using
Common Lisp.  You can use it to configure hosts as root, deploy services as
unprivileged users, build and deploy containers, and produce disc images.

Consfigurator's design gives you a great deal of flexibility about how to
control the hosts you want to configure.  Input and output streams attached to
an interactive POSIX sh running on the target host (or in the target
container) is sufficient to use much of Consfigurator's functionality.  But if
it is possible to get an implementation of Common Lisp started up on the host,
then Configurator can transparently execute your deployment code over on the
remote side, rather than exchanging information via POSIX sh.  This lets you
use the full power of Common Lisp to deploy your configuration.

Configurator has convenient abstractions for combining these different ways to
execute your configuration on hosts with different ways of connecting to them.
Connections can be arbitrarily nested.  For example, to combine SSHing to a
Debian machine as an unprivileged user, using sudo to become root, and then
starting up a Lisp process to execute your deployment code, you would just
evaluate ``(deploy (:ssh (:sudo :user "root") :debian-sbcl) foo.example.com)``.

Declarative configuration management systems like Consfigurator and Propellor_
share a number of goals with projects like the `GNU Guix System`_ and
`NixOS`_.  However, tools like Consfigurator and Propellor try to layer the
power of declarative and reproducible configuration on top of traditional,
battle-tested unix system administration infrastructure like apt, dpkg, yum,
and distro package archives, rather than seeking to replace any of those.
Let's get as much as we can out of all that existing distro policy-compliant
work!

.. _Propellor: https://propellor.branchable.com/
.. _GNU Guix System: https://guix.gnu.org/
.. _NixOS: https://nixos.org/

Quick start / introduction
==========================

1. Install Steel Bank Common Lisp and Consfigurator.  One way to do the latter
   is to clone this git repository into ``~/.local/share/common-lisp/source``.

2. Create a new directory ``consfig`` somewhere where ASDF will pick it up,
   such as ``~/common-lisp/consfig``.

3. Define a Lisp system which represents your configuration.

    ~/common-lisp/consfig/com.example.consfig.asd::

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

4. Define some hosts and connections.

    ~/common-lisp/consfig/consfig.lisp::

        (in-package :com.example.consfig)

        (setconsfig :com.example.consfig)

        (defhost athena.example.com
          "Web and file server."
          (file:contains-lines "/etc/default/locale" '("LANG=en_GB.UTF-8")))

        (defhostdeploy :ssh athena.example.com)

5. Get a Lisp REPL started up -- ``M-x slime`` in Emacs or ``sbcl`` at a shell
   prompt.  Evaluate ``(asdf:require-system "com.example.consfig")``.

6. Now you should be able to use configure athena by evaulating
   ``(com.example.consfig:athena.example.com)``.  You can use the
   ``CONSFIGURATOR:DEPLOY`` function to try out configuring athena using a
   different connection type than defined here.

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
Propellor supports something equivalent to a single, unnested ``:lisp``
connection -- and I wanted to implement that in Lisp.  Also, after five years
of using and extending Propellor, I've come to disagree with Joey about
whether Haskell's type system helps or hinders using and extending Propellor.

.. Propellor_: https://propellor.branchable.com/
