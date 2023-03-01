.. _introduction:

Introduction
============

Try it out / quick start
------------------------

1. Install Consfigurator (:ref:`Installation`).

2. Create a new directory ``consfig`` somewhere where ASDF will pick it up,
   such as ``~/common-lisp/consfig``.

3. Define a Lisp system which represents your configuration.

    ~/common-lisp/consfig/com.example.consfig.asd::

        (asdf:defsystem :com.example.consfig
          :serial t
          :depends-on (#:consfigurator #:cl-interpol)
          :components ((:file "package")
                       (:file "consfig")))

    ~/common-lisp/consfig/package.lisp::

        (in-package :cl-user)

	;; this macro is a simple wrapper of DEFPACKAGE which sets up local
	;; nicknames for packages providing properties and data sources
        (consfigurator:defpackage-consfig :com.example.consfig
          (:use #:cl #:alexandria #:consfigurator))

4. Define some hosts and deployments.

    ~/common-lisp/consfig/consfig.lisp::

        (in-package :com.example.consfig)
        (in-consfig "com.example.consfig")
	(named-readtables:in-readtable :consfigurator)

        (defhost athena.example.com (:deploy ((:ssh :user "root") :sbcl))
          "Web and file server."
	  (os:debian-stable "buster" :amd64)

	  (apt:mirrors "http://my.local.mirror.example.com/")
	  (apt:uses-local-cacher) ; sets up apt-cacher-ng
	  (apt:standard-sources.list)

	  ;; Set key--value pairs in INI-style files.  Consfigurator will
	  ;; uncomment existing lines in preference to adding new ones, for
	  ;; readability.
	  (file:contains-ini-settings "/etc/systemd/logind.conf"
	                              '("Login" "KillUserProcesses" "no"))

	  (apt:service-installed-running "apache2")

	  ;; Apply some properties as a non-root user.
	  ;; (as "spwhitton"
	  ;;   (gnupg:public-key-imported "8DC2 487E 51AB DD90 B5C4  753F 0F56 D055 3B6D 411B"))

	  (file:has-content "/etc/foo"
	    "Here is my file content.
	It's multiline.  CL-INTERPOL and CL-HEREDOC are also available; the
	latter is particularly useful for shell scripts.")
	  (file:has-content "/etc/bar" '("or" "specify" "a" "list" "of" "lines"))
	  (file:contains-lines "/etc/some.conf" "FOO=bar") ; preserve rest of file contents

	  ;; This will call debootstrap(1) in a way which respects the apt
	  ;; cacher and mirror configured above, so setting up multiple
	  ;; chroots with the same OS will be fast.
	  (chroot:os-bootstrapped. nil "/srv/chroot/test"
	    (os:debian-unstable :amd64)
	    (apt:standard-sources.list)

	    ;; These two properties are not for debootstrap(1) for but apt
	    ;; inside the chroot.
	    (apt:uses-parent-proxy) ; use the apt-cacher-ng set up outside chroot
	    (apt:uses-parent-mirrors))) ; use the apt mirror set up above

    This assumes that you have your SSH agent etc. set up such that you can
    ssh to root on athena without having to type a password into ssh's stdin.
    You should also enable SSH connection sharing, including ``ControlPersist``.

5. Get a Lisp REPL started up -- ``M-x slime`` in Emacs or ``sbcl`` at a shell
   prompt.  Evaluate ``(asdf:load-system "com.example.consfig")``, then
   ``(in-package :com.example.consfig)`` (or ``C-c ~`` in Emacs).

6. You should now be able to evaluate ``(athena.example.com)`` to deploy
   properties to athena, using the connection chain of SSH and then handing
   over to a remote Lisp image.

Other things to try
~~~~~~~~~~~~~~~~~~~

Note that some of these violate some of the ideas of declarative configuration
management, because they apply individual properties without updating the
definitions of hosts.  Sometimes that's the right thing to do, though, and
Consfigurator makes it easy to reuse your property definitions in these
non-declarative ways.

Try deploying properties to athena using a different connection type
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Evaluate something like::

  (deploy :ssh athena.example.com)

Apply a security update to all your systems
+++++++++++++++++++++++++++++++++++++++++++

It's useful to be able to quickly apply a security update across multiple
machines without otherwise interacting with their configuration.  Supposing
you have defined a variable ``*ALL-MY-SERVERS*`` which is a list hosts defined
with ``DEFHOST``, you can evaluate::

  (dolist (server *all-my-servers*)
    (deploy-these :ssh server
                  (cmd:single "apt-get update && apt-get upgrade openssl")))

Regex replace a file across hosts
+++++++++++++++++++++++++++++++++

With ``*ALL-MY-SERVERS*`` as in the previous example,

.. code-block:: none

  (dolist (server *all-my-servers*)
    (deploy-these :ssh server
                  (file:regex-replace-lines "/etc/baz" #?/^foo/ "bar")))

(relies on CL-INTERPOL syntax being enabled, as it is in the example consfig
above)

Concepts and terminology
------------------------

We make some simplifications.  More precise definitions appear later in this
manual.

Host
~~~~

A machine, container, chroot, or similar.  Has a plist of static informational
*host attributes* ("hostattrs"), usually including at least a hostname, and a
property application specification defining the properties it has.

Property
~~~~~~~~

Some configuration which a host can have or lack, and which can be added to
a host by running some code, possibly just by applying a series of other
properties.

For example: the presence of some lines in a config file; a package being
installed or absent; the availability of a website.

Connection
~~~~~~~~~~

A means by which properties can be applied to hosts, and multihop connections
to other hosts can be established.  There are two types of connections: those
which interact with the remote host by means of a POSIX shell, and those which
apply properties by executing them in a Lisp image running on the host.

POSIX connections can pass input to and return output from processes, but
cannot start asynchronous processes for interaction with your Lisp functions.
This is so that POSIX connections can be defined to control hosts for which
any kind of shell multiplexing is hard or impossible, such as with serial
connections providing only a single interactive POSIX sh.  For asynchronous
interaction, use a Lisp connection.

Deployment
~~~~~~~~~~

The combination of a connection and a host.  Executing a connection deploys
all of a host's usual properties to that host by means of the given
connection.  To deploy just a few particular properties, you can use
``DEPLOY-THESE``.

A deployment is itself a property.  This is one way in which connections can
be nested: one remote host can be used to deploy others, as a controller.

Root Lisp
~~~~~~~~~

The Lisp image you control directly when you execute deployments.  Typically
running on your development laptop/workstation (and not as the ``root`` user).

Property application specification ("propspec")
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A specification, in Consfigurator's DSL, for the properties that a host has
and/or lacks.  For example,::


  (eseqprops (apt:installed postfix)
             (etc-default:contains "locale" "LANG" "en_GB.UTF-8")
             (unapplied (com.example.consfig.services:mail-satellite)))

Property application specifications are applied in order, so properties later
in the list usually implicitly depend on properties earlier in the list,
though some property combinators can change this.

Unevaluated property application specification ("unevaluated propspec")
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A property application specification, except in atomic property applications
of the form ``(PROPERTY . ARGS)``, ``ARGS`` are expressions to be evaluated to
produce the arguments to pass to ``PROPERTY``, rather than those arguments
themselves.  An unevaluated property application specification can be
converted into a property application specification by evaluating each of
``ARGS``.

The main places you will find an unevaluated property application
specification is in calls to ``DEFHOST``, ``DEPLOY``, ``DEPLOY-THESE`` and
``DEFPROPLIST``.  Theses macros converts an unevaluated property application
specification into code which will produce the corresponding property
application specification.

Prerequisite data
~~~~~~~~~~~~~~~~~

File contents required to apply a property which should be generated or
extracted, by the root Lisp, at the time of deployment: a tarball containing
the latest version of the web service to be deployed; a secret extracted from
an encrypted store; a git bundle from localhost which the target host cannot
just ``git clone`` to itself.

Prerequisite data is versioned.  To replace a secret key, for example, you
change the data and bump the version.  If there is no version bump,
Consfigurator will assume connections can re-use old copies of prerequisite
data; this avoids uploading the same data over and over again.

In addition to secrets management, prerequisite data is Consfigurator's
mechanism for the common need to upload files to controlled hosts.  The same
mechanism is used internally to upload the Lisp code needed to start up remote
Lisp images for ``:lisp`` connections.

Consfig
~~~~~~~

An ASDF system in which you define your hosts and initialise sources of
prerequisite data.  This system might also define some site-specific
properties, default deployments, and helper functions.  Typically the system
is named ``COM.EXAMPLE.CONSFIG`` where ``example.com`` is your primary domain
name.

The system can contain multiple packages, perhaps to divide up your
definitions of hosts and default deployments from your site-specific
properties (e.g. you might have a package called
``COM.EXAMPLE.CONSFIG.SITES``).

You can have multiple independent Consfigs loaded into the root Lisp at once,
but if you do, then you should avoid using the ``*CONSFIG*`` global variable.

Documentation conventions
-------------------------

All unqualified names of Lisp symbols refer to those exported from the
``CONSFIGURATOR`` package, because it is assumed that this package is imported
unqualified into both user consfigs and Lisp packages providing properties,
connection types and sources of prerequisite data.

``FOO.BAR:BAZ`` means a symbol ``BAZ`` defined in
``CONSFIGURATOR.PROPERTY.FOO.BAR``, except that ``DATA.FOO:BAR`` means a
symbol ``BAR`` defined in ``CONSFIGURATOR.PROPERTY.DATA.FOO``.  These are the
recommended package nicknaming schemes for use in consfigs, e.g.::

  (defpackage :com.example.consfig
    (:use #:cl #:consfigurator)
    (:local-nicknames (#:file        #:consfigurator.property.file)
                      (#:cmd         #:consfigurator.property.cmd)
		      (#:data.pgp    #:consfigurator.data.pgp)))

You can use the ``DEFPACKAGE-CONSFIG`` macro to set up all these local
nicknames.

Portability and stability
-------------------------

- The core library should be portable between standards-conforming
  implementations of ANSI Common Lisp which include support for a few
  additional, widely-implemented features such as package-local nicknames.
  Optional packages providing properties and connection types might use
  implementation-specific functionality.  Little to no testing is done by the
  author on implementations other than SBCL, so testing and portability
  patches are welcome.

- Lisp implementations which will run on the hosts you wish to configure must
  support multithreading and must expose some mechanism for safely calling
  fork(2) in the presence of non-user threads, like ``SB-POSIX:FORK`` in the
  case of SBCL.  The root Lisp does not need to fork(2).  With some additional
  portability patches, it should be possible to host the root Lisp even on
  systems to which Consfigurator probably can't apply properties, such as
  Microsoft Windows.

- As both Consfigurator and its dependency Osicat make use of CFFI-Grovel,
  loading Consfigurator into Lisp currently always additionally requires a C
  toolchain, and development headers for libacl.  On GNU/Linux, development
  headers for libcap are also required.  It might be possible to
  conditionalise further so as to avoid any dependency on a C toolchain for
  the root Lisp.

- Little attempt is made by the author to support systems other than Debian
  GNU/Linux, but again, portability patches are welcome, and the design of
  Consfigurator should enable supporting other systems.

Credits
-------

Many of the good ideas here come straight from Joey Hess's Propellor_.  I'm
working on Consfigurator because I think Propellor is great, but wanted to add
Consfigurator's POSIX-type connections and arbitrary connection nesting, and I
wanted to implement that in Lisp (Propellor only supports something equivalent
to a single, unnested Lisp-type connection).  Additionally, after five years
of using and extending Propellor, I've come to disagree with Joey about
whether Haskell's type system helps or hinders using and extending Propellor.

.. _Propellor: https://propellor.branchable.com/
