(require "asdf")

;; this in itself ensures that we've listed the files in consfigurator.asd in
;; the correct order
(let ((asdf:*compile-file-failure-behaviour* :error)
      (asdf:*compile-file-warnings-behaviour* :error)
      (asdf:*user-cache* (uiop:getenv "AUTOPKGTEST_TMP")))
  (asdf:load-system "consfigurator/tests"))

;; we can't use ASDF:TEST-SYSTEM because its return value does not indicate
;; whether any tests failed
(unless (consfigurator/tests::do-tests)
  (uiop:quit 2))
