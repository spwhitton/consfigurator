(in-package :consfigurator/tests)
(named-readtables:in-readtable :consfigurator)
(in-consfig "consfigurator/tests")

(deftest read-heredoc.1
    #>EOF>yesEOF
  "yes")

(deftest read-heredoc.2
    #>>EOF>>yes
  yesEOF
  "  yes")

(deftest read-heredoc.3
    #>>EOF>> ; well
line 1
EOF
  "line 1
")

(deftest read-heredoc.4
    #>~EOF>    blah
    blah
    EOF
  "blah
blah
")

(deftest read-heredoc.5
    #>>~EOF>>
  line 1
  line 2
  EOF
  "line 1
line 2
")
