Introduction
============

Consfigurator is a system for declarative configuration management using
Common Lisp.  You can use it to configure hosts as root, deploy services as
unprivileged users, build and deploy containers, and produce disc images.

Consfigurator's design gives you a great deal of flexibility about how to
control the hosts you want to configure.  If there is a command you can run
which will obtain input and output streams attached to an interactive POSIX sh
running on the target host/container, then with a little glue code, you can
use much of Consfigurator's functionality to configure that host/container.
But if it is possible to get an implementation of Common Lisp started up on
the host, then Configurator can transparently execute your deployment code
over on the remote side, rather than exchanging information via POSIX sh.
This lets you use the full power of Common Lisp to deploy your configuration.

Configurator has convenient abstractions for combining these different ways to
execute your configuration on hosts with different ways of connecting to them.
Connections can be arbitrarily nested.  For example, to combine SSHing to a
Debian machine as an unprivileged user, using sudo to become root, and then
starting up a Lisp image to execute your deployment code, you would just
evaluate::

  (deploy ((:ssh (:sudo :as "spwhitton@athena.example.com") :debian-sbcl)) athena.example.com)

Declarative configuration management systems like Consfigurator and Propellor_
share a number of goals with projects like the `GNU Guix System`_ and
`NixOS`_.  However, tools like Consfigurator and Propellor try to layer the
power of declarative and reproducible configuration on top of traditional,
battle-tested unix system administration infrastructure like apt, dpkg, yum,
and distro package archives, rather than seeking to replace any of those.
Let's get as much as we can out of all that existing distro policy-compliant
work!

*Some features described in the foregoing are not yet implemented, but
Consfigurator's design permits them to be.*

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
might be large and relatively slow to run.  Hit ``C-c C-c`` on your
``DEFPROP`` form in Emacs, switch to the repl, and then use ``DEPLOY-THESE``
to run just that property against localhost or a local container, until it
does what it should.

For this purpose you can use whatever connection type is most convenient --
perhaps you normally deploy using Consfigurator's support for starting up
remote Lisp images, but you can swap in a simple, lighter-weight connection
type for testing.  Another respect in which this is useful is that interactive
debugging is not possible with connection types which start up remote Lisp
images.

We also have a few nice macros defined, though nothing too clever yet.

Installation and usage
======================

Please see the `user's manual`_ which includes a tutorial/quick start guide.

.. _user's manual: https://spwhitton.name/doc/consfigurator/

Bug reports, patches etc.
=========================

Please see CONTRIBUTING.rst, included in the source tree, for information
regarding the reporting of bugs and submission of patches/pull requests.

License
=======

Copyright (C) 2020-2021  Sean Whitton

Consfigurator is free software: you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation, either version 3 of the License, or (at your option) any later
version.

Consfigurator is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
details.

You should have received a copy of the GNU General Public License along with
Consfigurator.  If not, see <http://www.gnu.org/licenses/>.
