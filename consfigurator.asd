(asdf:defsystem :consfigurator
  :serial t
  :depends-on (#:cl-ppcre
	       #:alexandria)
  :components ((:file "package")
	       (:file "util")
	       (:file "core")
	       (:file "library")
	       (:file "connection/ssh")))
