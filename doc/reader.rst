Reader macros
=============

Loading Consfigurator defines the ``:CONSFIGURATOR`` named readtable.  Its
original purpose was to define a few reader macros to make Lisp more readily
usable for Unix systems administration, and as such it's helpful in consfigs.
We now have the broader aim of providing a readtable that renders Lisp more
useful for general Unix-style text manipulation.  To this end, the reader
macros we define are all inspired by Perl syntax.

Backwards compatibility
-----------------------

We don't expect to make incompatible changes to how these reader macros work,
except to make them work more like their Perl equivalents.  With this in mind,
some particular reservations are made for particular macros, as detailed below.

``#?``: Regexps & interpolation
-------------------------------

Sharp-question mark is the well-known CL-INTERPOL_ reader macro.

.. _CL-INTERPOL: https://edicl.github.io/cl-interpol/

``#~m//``: PCRE matching
------------------------

This provides an abbreviation for shell- and Perl-style regexp matching:

.. code-block:: none

  (#~m/b.+b/i "FooBarBaz") => "BarB"
  (#~m/b(.+)b/i "FooBarBaz") => #("ar")
  (mapcar #3~m/(\w+)(\W+)(\w+)/ '("one two" "three four" "five six"))
      => ("two" "four" "six")

Any delimiters supported by ``CL-INTERPOL`` may be used, and the ``m`` is
always optional.  Standard trailing options ``g``, ``i``, ``m``, ``s`` and
``x`` are meaningful.  There is also ``p``, which means to attempt to parse
the matched strings and substrings as numbers; if a substring cannot be parsed
as a number, it is returned unmodified.

The return value depends on the numeric argument before the tilde:

- ``#~m//``, with no argument, returns a vector of the substrings
  corresponding to the capture groups, or if there were no capture groups,
  just the whole matched string.

- ``#0~m//`` returns two values: the whole matched string, and a vector of
  capture group substrings.  (This is plain ``CL-PPCRE:SCAN-TO-STRINGS``.)

- ``#n~m//`` returns two values: the nth capture group's substring, and a
  vector of all the capture group substrings.

``#!~m//``: PCRE negative matching
----------------------------------

Equivalent to ``(not #~m//)``.

``#~s///``: PCRE substitution
-----------------------------

This provides an abbreviation for shell- and Perl-style regexp substitution:

.. code-block:: none

  (#~s/foo/bar/ "foobarbaz") => "foofoobaz"
  (mapcar #~s/:.+:/`\&`/ '(":Hello:" ":Goodbye:")) => ("`:Hello:`" "`:Goodbye:`")

Again, any delimiters supported by ``CL-INTERPOL`` may be used, and the same
trailing options, except for ``p``, are meaningful.  This is
``CL-PPCRE:REGEX-REPLACE`` or ``CL-PPCRE:REGEX-REPLACE-ALL``, which see
regarding return values.

``#>EOF>`` and ``#>>EOF>>``: Heredocs
-------------------------------------

Following ``#>EOF>``, all characters are read into a string until the next
literal ``EOF``.  You may use any string in place of ``EOF``, except that it
must not begin with a tilde or contain any whitespace, and for the sake of
future extension, it must not begin with a backwards slash or begin or end
with single or double quotation marks.

You can double up the ``>``, as in ``#>>EOF>>``, to skip the remainder of the
line on which the ``#>>EOF>>`` appears, starting the heredoc at the beginning
of the following line.  For the sake of future extension, the remainder of the
line after the ``#>>EOF>>`` must not contain anything other than a single-line
comment.

The specification of the terminating string may be preceded by a tilde, as in
``#>>~EOF>>``, to mean an indented heredoc:

.. code-block:: none

  (foo "argument 1" #>>~EOF>>
       My line 1.
       My line 2.
       EOF)

The function receives ``"My line 1.\nMy line 2.\n"``.

See also
--------

- `perlop(1) <https://perldoc.perl.org/perlop>`_

- `inferior-shell <https://cliki.net/inferior-shell>`_

- `Let Over Lambda ch. 4
  <https://letoverlambda.com/index.cl/guest/chap4.html>`_, which originally
  inspired ``#~m//`` and ``#~s///``.
