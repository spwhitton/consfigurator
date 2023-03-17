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

(deftest perl-tilde-reader.1
    (#~/bar/ "foo bar ")
  "bar")

(deftest perl-tilde-reader.2
    (#~/(f.*) (bar)/ "foo bar ")
  #("foo" "bar"))

(deftest perl-tilde-reader.3
    (#0~/(f.*) (bar)/ "foo bar ")
  "foo bar" #("foo" "bar"))

(deftest perl-tilde-reader.4
    (#2~/(f.*) (bar)/ "foo bar ")
  "bar" #("foo" "bar"))

(deftest perl-tilde-reader.5
    (#!~/bar/ "foo")
  t)

(deftest perl-tilde-reader.6
    (handler-case (let ((*readtable*
                          (named-readtables:find-readtable :consfigurator)))
                    (read-from-string "(#!/bar/ \"foo\")"))
      (simple-reader-error (err)
        (format nil (simple-condition-format-control err))))
  "Expected \"~\" following \"!\".")

(deftest perl-tilde-reader.7
    (#~/\w{2}/g "aa bb cc")
  ("aa" "bb" "cc"))

(deftest perl-tilde-reader.8
    (mapcar #~s/foo/bar/ '("foo" "bar"))
  ("bar" "bar"))

(deftest perl-tilde-reader.9
    (#~s/${(+ 1 1)}/${(+ 2 2)}/ "2")
  "4" t)

(deftest perl-tilde-reader.10
    (#~s/\w/\w/ "a")
  "w" t)

(deftest perl-tilde-reader.11
    (#~s/foo/bar/ "foo foo foo")
  "bar foo foo" t)

(deftest perl-tilde-reader.12
    (#~s/foo/bar/g "foo foo foo")
  "bar bar bar" t)

(deftest perl-tilde-reader.13
    (#~s/ \s\w d \w\s /!/ix "aDa bDa cDa")
  "aDa!cDa" t)

(deftest perl-tilde-reader.14
    (#~s[^(\d) ]{`\1` } "4 foo")
  "`4` foo" t)

(deftest perl-tilde-reader.15
    (#~s(\d)((\&\)\()) " 4 ")
  " (4)() " t)

(deftest perl-tilde-reader.16
    (#~s/foo/#bar#/ "foo")
  "#bar#" t)

(deftest perl-tilde-reader.17
    (#~s#foo#\#bar\## "foo")
  "#bar#" t)

(deftest perl-tilde-reader.18
    (#~s'foo'${bar}' "foo")
  "${bar}" t)

(deftest perl-tilde-reader.19
    (#~/\d+/p "1234")
  1234)

(deftest perl-tilde-reader.19
    (#~/(\d+)/p "1234")
  #(1234))

(deftest perl-tilde-reader.21
    (#~/\d+/gp "1234 6789")
  (1234 6789))

(deftest perl-tilde-reader.22
    (#0~/aa (\d+)/p "aa 1234")
  "aa 1234" #(1234))

(deftest perl-tilde-reader.22
    (#0~/aa (.+)/p "aa bbbb")
  "aa bbbb" #("bbbb"))

(deftest perl-tilde-reader.24
    (#0~/(\d+)../p "1234")
  1234 #(12))

(deftest perl-tilde-reader.25
    (#1~/(\d+)../p "1234")
  12 #(12))

(deftest perl-tilde-reader.26
    (#1~/(..)../p "aabb")
  "aa" #("aa"))

(deftest perl-tilde-reader.27
    (#~/d(.)?$/p "d")
  #(nil))
