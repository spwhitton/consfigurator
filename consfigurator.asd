(asdf:defsystem :consfigurator
  :serial t
  :depends-on (#:alexandria
	       #:cl-ppcre
	       #:cl-interpol)
  :components ((:file "src/package")
	       (:file "src/util")
	       (:file "src/core")
	       (:file "src/connection/ssh")
	       (:file "src/connection/local")
	       (:file "src/connection/debian-sbcl")
	       (:file "src/property/command")
	       (:file "src/property/file")
	       (:file "src/data/asdf")
	       (:file "src/data/pgp")))
