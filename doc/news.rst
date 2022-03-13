News
====

Some user-visible changes in versions of Consfigurator >1.0.0 are documented
here.  Version numbers have three components, ``major.minor.patch``, with the
following semantics:

- we increment ``major`` for a release which contains particularly significant
  new features, enhancements and/or reworkings, whether or not upgrading to
  the release will require changes to user consfigs (though usually it will);

  + Additionally, while ``major`` is zero, we will be much more willing to
    make breaking changes.

- we increment ``minor`` for a release which does not satisfy the requirements
  for incrementing ``major``, but where there are changes that could require
  changes in user consfigs, except very obscure such consfigs; and

- we increment only ``patch`` for a release which includes no changes that we
  think could require changes in user consfigs.

Note that the notion of consfig-breaking changes is more than just strict API
breaks, but also changes in behaviour which will likely require review by
sysadmins using Consfigurator to maintain their systems.

In summary, you should always be able to upgrade to a release which only
increments ``patch``, but if either of the other two components have changed,
you should review this document and see if your consfig needs updating.

..
   1.0.1 (unreleased)
   ------------------