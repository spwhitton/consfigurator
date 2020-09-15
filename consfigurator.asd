(asdf:defsystem :consfigurator
  :serial t
  :depends-on (:cl-ppcre)
  :components ((:file "package")
	       (:file "util")
	       (:file "core")))
