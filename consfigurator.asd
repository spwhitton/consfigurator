(asdf:defsystem :consfigurator
  :serial t
  :depends-on (#:cl-ppcre
	       #:alexandria
	       #:cl-interpol)
  :components ((:file "src/package")
	       (:file "src/util")
	       (:file "src/core")
	       (:file "src/connection/ssh")))
