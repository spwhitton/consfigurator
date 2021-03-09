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
