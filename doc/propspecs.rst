Property application specifications
===================================

Combinators
-----------

Currently supported: ``(unapply (foo 1 2 3))``, ``((foo 1 2 3) on-change (bar
4 5 6) on-change (baz 7 8 9))`` and combinations thereof.

Unevaluated property application specifications
-----------------------------------------------

In an atomic property application within an unevaluated property application
specification, if the symbol naming the property ends with the character
``.``, then the following special evaluation rules apply:

1. The property to be applied is the property named by the symbol in the same
   package and with the same name as the first element of the atomic property
   application, but with the trailing period removed from the name.

2. The first argument is not evaluated if it is a list whose first element is
   a keyword, or a if it is a list of lists where the first element of the
   first list is a keyword.

3. The last argument is treated as an unevaluated property application
   specification and is converted into a property application specification
   according to the usual evaluation rules.

This is intended to make applications of properties like DEPLOYS,
DEPLOYS-THESE and CHROOT:DEBOOTSTRAPPED, which take property application
specifications as arguments, easier to read and write in the most common
cases.  For example, you can write::

  (deploys. (:ssh (:sudo :as "spwhitton@athena.example.com")) athena.example.com
    ((additional-property val1)
     (a-further-property val2)))

instead of::

  (deploys '(:ssh (:sudo :as "spwhitton@athena.example.com")) athena.example.com
    (make-propspec :props `((additional-property ,val1)
                            (a-further-property  ,val2))))

(though note the parentheses around the two property applications remain,
unlike in ``DEPLOY``, ``DEFHOST`` etc.).
