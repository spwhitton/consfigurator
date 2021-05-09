Tutorial conventions
--------------------

In these tutorials we assume that you have a workstation called
``laptop.silentflame.com`` where you run the root Lisp.  We also assume that
Consfigurator knows about your laptop, and that it has a host deployment
specified, so that you can use ``HOSTDEPLOY-THESE`` to deploy properties to
the laptop as root.  For example,::

  (defhost laptop.silentflame.com
      (:deploy ((:sudo :as "spwhitton@melete.silentflame.com") :sbcl))
    "Sean's laptop."
    (os:debian-stable "bullseye" :amd64))

We suppose that you've already set up sources of prerequisite data to provide
sudo passwords and the like.  See the :ref:`introduction<introduction>` if you
haven't set this up yet.
