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
starting up a Lisp image to execute your deployment code, you would just
evaluate::

  (deploy (:ssh (:sudo :as "spwhitton@athena.example.com") :debian-sbcl) athena.example.com)

Declarative configuration management systems like Consfigurator and Propellor_
share a number of goals with projects like the `GNU Guix System`_ and
`NixOS`_.  However, tools like Consfigurator and Propellor try to layer the
power of declarative and reproducible configuration on top of traditional,
battle-tested unix system administration infrastructure like apt, dpkg, yum,
and distro package archives, rather than seeking to replace any of those.
Let's get as much as we can out of all that existing distro policy-compliant
work!

*Some features described in the foregoing are not yet implemented, but
Consfigurator's design should permit them to be soon.*

.. _Propellor: https://propellor.branchable.com/
.. _GNU Guix System: https://guix.gnu.org/
.. _NixOS: https://nixos.org/

About the name
--------------

``CONS`` is a fundamental operator in Lisp.  Consfigurator is so named because
we hope to enable configuration management workflows which take advantage of
some of the unique properties of the activity of programming in Lisp.

For example, using Lisp's interactivity, it's easy to test a new property
you're working on without having to plumb it into your main deployments, which
might be large and relatively slow to run.  Hit C-c C-c on your ``DEFPROP``
form in Emacs, switch to the repl, and then use ``DEPLOY-THESE`` to run just
that property against localhost or a local container, until it does what it
should.  For this purpose you can use whatever connection type is most
convenient -- perhaps you normally deploy using Consfigurator's support for
starting up remote Lisp images, but you can swap in a simple, lighter-weight
connection type for testing.

We also have a few nice macros defined, though nothing too clever yet.

Try it out / quick start
========================

1. ``apt-get install sbcl cl-ppcre cl-interpol cl-alexandria cl-babel``, or
   ``(ql:quickload "cl-ppcre")``, ``(ql:quickload "alexandria")``,
   ``(ql:quickload "cl-interpol")`` and ``(ql:quickload "babel")`` (see
   https://www.quicklisp.org/).

2. Install Consfigurator.  One way to do that is to clone this git repository
   into ``~/.local/share/common-lisp/source``.

3. Create a new directory ``consfig`` somewhere where ASDF will pick it up,
   such as ``~/common-lisp/consfig``.

4. Define a Lisp system which represents your configuration.

    ~/common-lisp/consfig/com.example.consfig.asd::

        (asdf:defsystem :com.example.consfig
          :serial t
          :depends-on (#:consfigurator #:cl-interpol)
          :components ((:file "package")
                       (:file "consfig")))

    ~/common-lisp/consfig/package.lisp::

        (in-package :cl-user)

        (defpackage :com.example.consfig
          (:use #:cl #:consfigurator #:alexandria)
          (:local-nicknames (#:file      #:consfigurator.property.file)
                            (#:cmd       #:consfigurator.property.cmd)
                            (#:data.pgp  #:consfigurator.data.pgp)))

5. Define some hosts and deployments.

    ~/common-lisp/consfig/consfig.lisp::

        (in-package :com.example.consfig)
        (in-consfig "com.example.consfig")
	(named-readtables:in-readtable :interpol-syntax)

	(try-register-data-source
         :pgp :location #P"/path/to/com.example.consfig.gpg")

        (defhost athena.example.com
          "Web and file server."
	  (file:has-content "/etc/foo" '("these" "are" "my" "lines"))
	  (file:contains-lines "/etc/some.conf" '("FOO=bar")))

        (defhostdeploy (:ssh (:sudo :as "spwhitton@athena.example.com") :debian-sbcl)
	               athena.example.com)

    Here, "spwhitton" is my username on athena; we have to tell Consfigurator
    what user it will be when it tries to sudo, so it knows whose password it
    needs.  If you have passwordless sudo access configured, you can skip the
    ``:AS`` keyword parameter and its argument.

6. Get a Lisp REPL started up -- ``M-x slime`` in Emacs or ``sbcl`` at a shell
   prompt.  Evaluate ``(asdf:load-system "consfigurator")``.

7. When it's asked to use sudo to become root, Consfigurator will query your
   registered sources of secrets to try to find the password it will need to
   give to sudo.  You can easily write code to let Consfigurator query your
   own sources of secrets, but for the purposes of this guide we'll use the
   simple, PGP-based secrets source included with Consfigurator.  Unless
   you've passwordless sudo access set up on athena, evaluate something like
   this to initialise the store::

     (consfigurator.data.pgp:set-data #P"/path/to/com.example.consfig.gpg"
                                      "--user-passwd--athena.example.com"
				      "spwhitton"
				      "s3cre+")

8. Now you can evaluate ``(asdf:load-system "com.example.consfig")`` followed
   by ``(in-package :com.example.consfig)`` (or ``C-c ~`` in Emacs).  In the
   future, now the secrets store exists, you can start with this step.

9. You should now be able to evaluate ``(athena.example.com)`` to deploy
   properties to athena, using the connection chain of SSH, sudo and then
   handing over to a remote Lisp image.

Other things to try
-------------------

Note that some of these violate some of the ideas of declarative configuration
management, because they apply individual properties without updating the
definitions of hosts.  Sometimes that's the right thing to do, though, and
Consfigurator makes it easy to reuse your property definitions in these
non-declarative ways.

Try deploying properties to athena using a different connection type
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Evaluate something like::

  (deploy :ssh athena.example.com)

Apply a security update to all your systems
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

It's useful to be able to quickly apply a security update across multiple
machines without otherwise interacting with their configuration.  Supposing
you have defined a variable ``*ALL-MY-SERVERS*`` which is a list hosts defined
with ``DEFHOST``, you can evaluate::

  (dolist (server *all-my-servers*)
    (deploy-these :ssh server
                  (cmd:single "apt-get update && apt-get upgrade openssl")))

Regex replace a file across hosts
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

With ``*ALL-MY-SERVERS*`` as in the previous example,::

  (dolist (server *all-my-servers*)
    (deploy-these :ssh server
                  (file:regex-replace-lines "/etc/baz" #?/foo/ "bar")))

(relies on CL-INTERPOL syntax being enabled, as it is in the example consfig
above)

Portability and stability
=========================

- **Consfigurator is still stabilising and so there may be lots of breaking
  changes.**

- All of the code should be portable ANSI Common Lisp, but little to no
  testing is done by the author on implementations other than SBCL, so testing
  and portability patches are welcome.

- Little attempt is made by the author to support systems other than Debian
  GNU/Linux, but again, portability patches are welcome, and the design of
  Consfigurator should enable supporting other systems.

Bug reports, patches etc.
=========================

Please see the included CONTRIBUTING.rst.

Credits
=======

Many of the good ideas here come straight from Joey Hess's Propellor_.  I'm
working on Consfigurator because I think Propellor is great, but wanted to add
Consfigurator's POSIX-type connections and arbitrary connection nesting, and I
wanted to implement that in Lisp (Propellor only supports something equivalent
to a single, unnested Lisp-type connection).  Additionally, after five years
of using and extending Propellor, I've come to disagree with Joey about
whether Haskell's type system helps or hinders using and extending Propellor.

.. Propellor_: https://propellor.branchable.com/
