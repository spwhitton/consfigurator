(in-package :consfigurator/tests)
(named-readtables:in-readtable :consfigurator)
(in-consfig "consfigurator/tests")

(defun populate-data-pass ()
  "Invoked by test runner before data source is registered."
  (pass '("insert" "-m" "server.example.org/account") :input "hunter2")
  (pass '("insert" "-m" "_foo/bar/baz") :input "OK")
  (pass '("insert" "-m" "foo/bar/baz") :input "illegal")
  (pass '("insert" "-m" "valid/file") :input "shadowed")
  (pass '("insert" "-m" "_valid/file") :input "visible")
  (pass '("insert" "-m" "toplevel") :input "sekrit")
  (pass '("insert" "-m" "server.example.org/etc/foo.conf")
        :input "[section]
key=value"))

(deftest pass-host.1
    (get-data-string "server.example.org" "account")
  "hunter2")

(deftest pass-host.2
    (get-data-string "--user-passwd--server.example.org" "account")
  "hunter2")

(deftest pass-host.3
    (get-data-string "server.example.org" "/etc/foo.conf") "[section]
key=value")

(deftest pass-host.4
    (handler-case
        (get-data-string "a.example.com" "/etc/foo.conf")
      (missing-data (c) "fail"))
  "fail")

(deftest pass-underscore.1
    (get-data-string "_server.example.org" "account")
  "hunter2")

(deftest pass-underscore.2
    (get-data-string "_foo/bar" "baz") "OK")

(deftest pass-underscore.3
    (handler-case
        (get-data-string "foo/bar" "baz")
      (simple-program-error (c) "fail"))
  "fail")

(deftest pass-underscore.4
    (get-data-string "_valid" "file") "visible")

(deftest pass-underscore.5
    (get-data-string "_" "toplevel") "sekrit")
