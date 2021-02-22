(asdf:defsystem :consfigurator
  :serial t
  :depends-on (#:alexandria
	       #:babel
	       #:babel-streams
	       #:cl-ppcre
	       #:cl-interpol)
  :components ((:file "src/package")
	       (:file "src/util")
	       (:file "src/connection")
	       (:file "src/property")
	       (:file "src/propspec")
	       (:file "src/host")
	       (:file "src/deployment")
	       (:file "src/connection/local")
	       (:file "src/data")
	       (:file "src/connection/ssh")
	       (:file "src/connection/debian-sbcl")
	       (:file "src/property/command")
	       (:file "src/property/file")
	       (:file "src/data/asdf")
	       (:file "src/data/pgp")))
