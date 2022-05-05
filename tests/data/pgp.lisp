(in-package :consfigurator/tests)
(named-readtables:in-readtable :consfigurator)
(in-consfig "consfigurator/tests")

(defun populate-data-pgp ()
  "Invoked by test runner before data source is registered."
  (data.pgp:set-data *test-pgp-file* "_secrets" "test" "this is a sekrit")
  (data.pgp:set-data *test-pgp-file* "host.example.com" "/etc/foo.conf"
                     "secret file content"))

(deftest data.pgp.1
    (data.pgp:get-data *test-pgp-file* "_secrets" "test")
  "this is a sekrit")

(deftest data.pgp.2
    (get-data-string "_secrets" "test")
  "this is a sekrit")

(deftest data.pgp.3
    (get-data-string "host.example.com" "/etc/foo.conf")
  "secret file content")
