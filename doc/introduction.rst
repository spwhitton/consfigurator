Introduction
============

This is the user's guide for Consfigurator.

Concepts and terminology
------------------------

Host
~~~~

A machine, container, chroot, or similar.  Has a plist of static informational
*host attributes*, usually including at least a hostname, and a property
application specification defining the properties it has.

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
apply properties by executing them in a Lisp process running on the host.

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

The Lisp process you control directly when you execute deployments.  Typically
running on your development laptop/workstation (and not as the ``root`` user).

Property application specification
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

An ordered list specifying the properties that a host has and/or lacks.  For
example,::

  '((apt:installed postfix)
    (etc-default:set "locale" "LANG" "en_GB.UTF-8")
    (unapply (com.example.consfig.services:mail-satellite)))

Property application specifications are always applied in order, so properties
later in the list implicitly depend on properties earlier in the list.

Unevaluated property application specification
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A property application specification, except in atomic property applications
of the form ``(PROPERTY . ARGS)``, ``ARGS`` are expressions to be evaluated to
produce the arguments to pass to ``PROPERTY``, rather than those arguments
themselves.  An unevaluated property application specification can be
converted into a property application specification by evaluating each of
``ARGS``.

The main place you will find an unevaluated property application specification
is in a call to ``DEFHOST``.  That macro converts an unevaluated property
application specification into code which will produce the corresponding
property application specification.

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
Lisp processes for ``:lisp`` connections.

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
