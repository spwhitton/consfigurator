Prerequisite data
=================

Naming
------

A piece of prerequisite data is identified by two strings.  Typically the
first of these specifies the context in which the data is relevant.  For an
ssh host key, for example, this context would be a hostname.  If it's ``nil``
then the data is valid in any context.  The second of these identifies the
data within its context.  This is often just the filename in which the
prerequisite data will eventually be stored.  It might also be a
human-readable string describing the purpose of the data.

Reserved names
~~~~~~~~~~~~~~

These are exclusive semantics for certain possible pairs of strings
identifying prerequisite data -- to avoid confusion and potential clashes, do
not use prerequisite data identified by strings matching these conditions for
other purposes.

- ``(HOSTNAME . PATH)`` means the data that should be uploaded to ``PATH`` on
  ``HOSTNAME`` (and nowhere else)

- ``("--lisp-system" . SYSTEM)`` means the data is Lisp code which, when
  loaded, defines the packages and symbols contained in the ASDF system
  ``SYSTEM``

- ``("--user-passwd--HOSTNAME" . USER)`` means the data is the password for
  user ``USER`` on ``HOSTNAME``

- ``("--git-snapshot" . NAME)`` means the data is a snapshot of a git repo
  identified by ``NAME``; see ``DATA.GIT-SNAPSHOT``

- ``("--pgp-pubkey" . FINGERPRINT)`` means the/a OpenPGP public key with
  fingerprint FINGERPRINT, ASCII-armoured

- ``("--pgp-seckey" . FINGERPRINT)`` means the/a OpenPGP secret key with
  fingerprint FINGERPRINT, ASCII-armoured

- ``("--luks-passphrase" . VOLUME-LABEL)`` means a LUKS passphrase for volume
  with label ``VOLUME-LABEL``.

(Proposed convention: Except for the first item above, these reserved names
should start with ``--`` and use ``--`` to separate parameter values within
the string.  Hostnames cannot start with a hyphen.)

Mechanics
---------

Properties declare that they need certain pieces of prerequisite data by
adding static informational attributes, and a deployment of those properties
will make an attempt to provide the data.  Properties then either call the
``GET-DATA-STREAM`` function or the ``GET-DATA-STRING`` function, or depend on
the ``DATA-UPLOADED`` property, to get access to the requested data.

A Lisp connection gathers all needed prerequisite data once at the beginning,
and copies it to an on-disk cache inside the home directory of the remote UID
which will run the Lisp image.  A POSIX connection only attempts to obtain
prerequisite data when a property's check function indicates the property is
not already applied.

Sources of prerequisite data
----------------------------

Sources of prerequisite data register two functions.  The second returns
either a string of the prerequisite data itself, or a path to a file
containing the data.  The first returns the latest version number of the data
that source is able to provide -- i.e., the version number of the data that
the second function would return if called.

Consfigurator will call the first function to find out if it needs to call the
first rather than just using its caches.  The first function should return nil
if it can't obtain the prerequisite data on this host, perhaps because it
can't decrypt the store.  If a prerequisite data source wants to effectively
bypass caching and provide fresh data every time Consfigurator deploys the
host, it can use ``GET-UNIVERSAL-TIME`` as its first function.

Versions are compared using ``UIOP:VERSION<`` and ``UIOP:VERSION<=``.

Security issues
---------------

Nothing is done to prevent prerequisite data being swapped out, so ensure your
swap is encrypted.

Certain connection types require storing unencrypted copies of prerequisite
data under ``~/.cache/consfigurator/data``.  Consfigurator only stores data
there when it has to, only the subset of the data that has to be uploaded for
the requested deployment to be successful, and never in the root Lisp.
