Hosts
=====

The HOSTATTRS list
------------------

This is a plist of lists, such that for each keyword symbol identifying a type
of static informational attribute ("hostattr"), there is a list of entries.
Property ``:HOSTATTRS`` subroutines may only push new entries to the front of
each such sublist, using the function ``PUSH-HOSTATTR``.  Use
``GET-HOSTATTRS`` and ``GET-HOSTATTRS-CAR`` to access the lists.

The relationship between older and newer entries in the sublist for each type
of static informational attribute is attribute-dependent.  For example, for
the ``:DATA`` attribute, the order of entries does not matter and each item is
equally a piece of prerequisite data required by the host's properties.  For
other kinds of attribute, it might be that later entries supercede earlier
ones, or that the entries should be combined in some way.  Property ``:APPLY``
subroutines decide how to interpret each type of static informational
attribute.

Reserved names for static informational attributes
--------------------------------------------------

For attributes that will only be used among a co-ordinating group of
properties, use a non-keyword symbol, whose package is one in which some or
all of those properties are defined.  This minimises the risk of any clashes.
Many attributes, however, will be shared across properties, and should use
keyword symbols.  The semantics of these attributes are documented here:

- ``:HOSTNAME``: the host's hostname -- if the host has a domain name, then
  the FQDN, not just the part before the first dot

- ``:ALIASES``: see ``NETWORK:ALIASES``

- ``:IPV4``: the host's public IPv4 addresses

- ``:IPV6``: the host's public IPv6 addresses

- ``:DATA``: items of prerequisite data required by the host

- ``:OS``: the operating system of the host

- ``:APT.MIRROR``: for hosts running Debian or a Debian derivative, the host's
  preferred apt mirror

Host designators
----------------

A string designates a host with that hostname and no properties.  Using
strings to designate hosts is not valid in all contexts -- some macros and
properties where it might be useful to pass a string instead of a ``HOST``
object call ``ENSURE-HOST`` to convert, but this is not done everywhere.
