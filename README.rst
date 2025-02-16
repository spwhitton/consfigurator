Introduction
============

Consfigurator is a system for declarative configuration management using
Common Lisp.  You can use it to configure hosts as root, deploy services as
unprivileged users, build and deploy containers, install operating systems,
produce disc images, and more.  Some key advantages:

- Apply configuration by transparently starting up another Lisp image on the
  machine to be configured, so that you can use the full power of Common Lisp
  to inspect and control the host.

- Also define properties of hosts in a more restricted language, that of
  ``:POSIX`` properties, to configure machines, containers and user accounts
  where you can't install Lisp.  These properties can be applied using just an
  SSH or serial connection, but they can also be applied by remote Lisp
  images, enabling code reuse.

- Flexibly chain and nest methods of connecting to hosts.  For example, you
  could have Consfigurator SSH to a host, sudo to root, start up Lisp, use the
  setns(2) system call to enter a Linux container, and then deploy a service.
  Secrets, and other prerequisite data, are properly passed along.

- Combine declarative semantics for defining hosts and services with a
  multiparadigmatic general-purpose programming language that won't get in
  your way.

Declarative configuration management systems like Consfigurator and Propellor_
share a number of goals with projects like the `GNU Guix System`_ and
`NixOS`_.  However, tools like Consfigurator and Propellor try to layer the
power of declarative and reproducible configuration semantics on top of
traditional, battle-tested UNIX system administration infrastructure like
distro package managers, package archives and daemon configuration mechanisms,
rather than seeking to replace any of those.  Let's get as much as we can out
of all that existing distro policy-compliant work!

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

We have a few nice macros defined, too.

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

| Copyright (C) 2015-2018, 2020-2025 Sean Whitton
| Copyright (C) 2021-2022 David Bremner

Consfigurator is free software: you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation, either version 3 of the License, or (at your option) any later
version.

Consfigurator is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
details.

You should have received a copy of the GNU General Public License along with
Consfigurator.  If not, see <https://www.gnu.org/licenses/>.
