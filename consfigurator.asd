(asdf:defsystem :consfigurator
  :serial t
  :depends-on (#:cl-ppcre
	       #:alexandria
	       #:cl-interpol)
  :components ((:file "package")
	       (:file "util")
	       (:file "core")
	       (:file "connection/ssh")))
