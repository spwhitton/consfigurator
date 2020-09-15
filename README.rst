Introduction
============

Consfigurator is a system for declarative configuration management using
Common Lisp.  You can use it to configure hosts as root, deploy services as
unprivileged users, and build disc images.

Many or all of the good ideas here come straight from Joey Hess's Propellor_.
I'm working on Consfigurator mainly because I think Propellor is great and
reimplementing those ideas is good practice in writing Common Lisp, but also
because after five years of using and extending Propellor, I've come to
disagree with Joey about whether Haskell's type system helps or hinders using
and extending Propellor.

.. Propellor_ https://propellor.branchable.com/

Portabiilty and stability
=========================

- **Consfigurator is still stabilising and so there may be breaking changes.**

- No attempt is made to support Common Lisp implementations other than SBCL,
  though portability patches are welcome.

- No attempt is made to support running on Windows -- we eschew Common Lisp
  pathnames in favour of simple strings with forward slashes as directory
  separators.
