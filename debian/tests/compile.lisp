(require "asdf")

;; this ensures that we've listed the files in consfigurator.asd in the
;; correct order

(let ((asdf:*compile-file-failure-behaviour* :error)
      (asdf:*compile-file-warnings-behaviour* :error)
      (asdf:*user-cache* (uiop:getenv "AUTOPKGTEST_TMP")))
  (asdf:load-system "consfigurator"))
