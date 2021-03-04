(asdf:defsystem :consfigurator
  :description "Lisp declarative configuration management system"
  :version "0.2.1"
  :author "Sean Whitton <spwhitton@spwhitton.name>"
  :licence "GPL-3+"
  :serial t
  :depends-on (#:alexandria
	       #:babel
	       #:babel-streams
	       #:cl-ppcre
	       #:cl-interpol
	       #:cffi)
  :components ((:file "src/package")
	       (:file "src/util")
	       (:file "src/connection")
	       (:file "src/property")
	       (:file "src/propspec")
	       (:file "src/host")
	       (:file "src/deployment")
	       (:file "src/connection/local")
	       (:file "src/data")
	       (:file "src/connection/shell-wrap")
	       (:file "src/connection/ssh")
	       (:file "src/connection/sudo")
	       (:file "src/connection/debian-sbcl")
	       (:file "src/connection/chroot/fork")
	       (:file "src/property/cmd")
	       (:file "src/property/file")
	       (:file "src/property/chroot")
	       (:file "src/data/asdf")
	       (:file "src/data/pgp")))
