Submitting patches
==================

Thank you for your interest in contributing to this project!

Please **do not** submit a pull request on GitHub.  The repository
there is an automated mirror, and I don't develop using GitHub's
platform.

Project mailing lists
=====================

There are two low-volume project mailing lists, shared with some other
small free software projects:

- sgo-software-announce --
  <https://www.chiark.greenend.org.uk/mailman/listinfo/sgo-software-announce>

  For release announcements.

- sgo-software-discuss --
  <https://www.chiark.greenend.org.uk/mailman/listinfo/sgo-software-discuss>

  For bug reports, posting patches, user questions and discussion.

Please prepend ``[consfigurator]`` to the subject line of your e-mail, and for
patches, pass ``--subject-prefix="PATCH consfigurator"`` to git-send-email(1).

Posting to sgo-software-discuss
-------------------------------

If you're not subscribed to the list, your posting will be held for
moderation; please be patient.

Whether or not you're subscribed, chiark.greenend.org.uk has
aggressive antispam.  If your messages aren't getting through, we can
easily add a bypass on chiark; please contact <spwhitton@spwhitton.name>.

If you don't want to deal with the mailing list and just want to send
patches, you should feel free to pass ``--to=spwhitton@spwhitton.name``
to git-send-email(1).

Alternatively, publish a git branch somewhere publically accessible (a
GitHub fork is fine) and write to me asking me to merge it.  I may
convert your branch back into patches when sending you feedback :)

IRC channel
===========

You can ask questions and discuss Consfigurator in ``#consfigurator`` on
server ``irc.oftc.net``.

Reporting bugs
==============

Please read "How to Report Bugs Effectively" to ensure your bug report
constitutes a useful contribution to the project:
<https://www.chiark.greenend.org.uk/~sgtatham/bugs.html>

Code style
==========

- Stick to a line width of 78.  An exception is made for string literals like
  error messages.

- Choose judiciously between a functional programming style and LOOP.  Err on
  the side of LOOP.

- Use SETQ not SETF whenever SETQ works.

- Separate sentences with two spaces after the period.  In recent Emacs there
  is ``M-x repunctuate-sentences`` to help with this.

- In docstrings, comments and commit messages, refer to symbols and Lisp
  packages -- though not ASDF systems -- in BLOCK CAPITALS, without any
  quotation marks.

- Follow the conventions detailed in sections "Tips for Documentation Strings"
  and "Tips on Writing Comments" of the *GNU Emacs Lisp Reference Manual*, to
  the extent that they are applicable to Common Lisp.  Additionally:

  - Place blank lines before and after comments whose lines begin with more
    than two semicolons.

  - Separate major sections of files using the form feed character, like
    this::

      ^L
      ;;;; Header for new major section

  - If the first major section of the file has a four-semicolon header, it
    should not be preceded by the ``^L`` character.

- Avoid complicating property definitions to include updates to file
  ownership, and the like, when it's possible instead to switch to the target
  user and apply a simpler version of the property.  For example, instead of
  taking a username as a parameter and then changing the ownership of any
  newly created files to the named user, you can use the ``AS`` combinator to
  apply the property as that user in the first place.

Signing off your commits
========================

Contributions are accepted upstream under the terms set out in the
file ``COPYING``.  You must certify the contents of the file
``DEVELOPER-CERTIFICATE`` for your contribution.  To do this, append a
``Signed-off-by`` line to end of your commit message.  An easy way to
add this line is to pass the ``-s`` option to git-commit(1).  Here is
an example of a ``Signed-off-by`` line:

::

    Signed-off-by: Sean Whitton <spwhitton@spwhitton.name>
