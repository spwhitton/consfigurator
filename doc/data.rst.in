Prerequisite data
=================

Naming
------

An item of prerequisite data is identified by two strings, called ``IDEN1``
and ``IDEN2``; together these are the *prerequisite data identifiers* for an
item of prerequisite data.  Typically ``IDEN1`` specifies the context in which
the data is relevant, and ``IDEN2`` identifies the data within its context.
``IDEN2`` is very often the filename in which the prerequisite data will
eventually be stored.  It might also be a human-readable string describing the
purpose of the data.  The following are the valid forms of ``IDEN1``, and
their meanings.

- ``(HOSTNAME . PATH)`` means the data that should be uploaded to ``PATH`` on
  ``HOSTNAME`` (and usually nowhere else, except in the case of, e.g., a
  public key).  ``PATH`` must be absolute, not relative.

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

- Any other ``IDEN1`` beginning with exactly two hyphens is reserved for
  future use.

- ``(_CONTEXT . ITEM)`` is an arbitrary prerequisite data context named
  ``CONTEXT``; typically ``CONTEXT`` will be a network or grouping name,
  rather than referring to a single host.  ``ITEM`` might be a path or some
  other identifier.  Reserved for consfigs; will not be used by property
  definitions included with Consfigurator, and should not be used by third
  party extensions.

- ``(---CONTEXT . ITEM)`` is, similarly, an arbitrary prerequisite data
  context named ``CONTEXT``.  This form is intended for contexts similar to
  the reserved names beginning with two hyphens: types of information rather
  than site-local network or grouping names.  This form will not be used by
  property definitions included with Consfigurator, but may be used by both
  consfigs and third party extensions.

Any other forms are invalid.  In particular, an ``IDEN1`` that is not a valid
hostname and does not begin with a hyphen or an underscore must not be used.

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
