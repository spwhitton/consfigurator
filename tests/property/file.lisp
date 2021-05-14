(in-package :consfigurator/tests)
(named-readtables:in-readtable :consfigurator)
(in-consfig "consfigurator/tests")

(deftest contains-conf-space
    (with-temporary-file (:pathname temp)
      (with-open-file (s temp :if-exists :supersede :direction :output)
        (write-sequence #>EOF>foo bar

# one two
one three
# one three
EOF s))
      (deploy-these :local "localhost"
        (evals `(file:contains-conf-space ,temp "foo" "baz" "one" "four" "two" "five")))
      (read-file-string temp))
  #>EOF>foo baz

one four
# one three
# one three
two five
EOF)

(deftest contains-conf-shell
    (with-temporary-file (:pathname temp)
      (with-open-file (s temp :if-exists :supersede :direction :output)
        (write-sequence #>EOF>foo="bar baz"

# bar=baz
other="three word s"
EOF s))
      (deploy-these :local "localhost"
        (evals `(file:contains-conf-shell
                 ,temp "bar" "two words" "quux" "one" "other" "one")))
      (read-file-string temp))
  #>EOF>foo="bar baz"

bar="two words"
other=one
quux=one
EOF)

(deftest contains-ini-settings
    (with-temporary-file (:pathname temp)
      (with-open-file (s temp :if-exists :supersede :direction :output)
        (write-sequence #>EOF>[one]
two=three

[other]
; bar = ba
bar = baz baz quux
EOF s))
      (deploy-these :local "localhost"
        (evals `(file:contains-ini-settings ,temp
                                            '("one" "four" "five")
                                            '("another" "k" "v")
                                            '("other" "bar" "baz")
                                            '("another" "key" "val")
                                            '("finally" "this" "one"))))
      (read-file-string temp))
  #>EOF>[one]
two=three
four=five

[other]
bar=baz
# bar=baz baz quux

[another]
k=v
key=val

[finally]
this=one
EOF)
