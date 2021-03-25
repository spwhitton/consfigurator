(asdf:defsystem :consfigurator
  :description "Lisp declarative configuration management system"
  :version "0.3.1"
  :author "Sean Whitton <spwhitton@spwhitton.name>"
  :licence "GPL-3+"
  :serial t
  :depends-on (#:alexandria
               #:babel
               #:babel-streams
               #:cl-ppcre
               #:cl-heredoc
               #:cl-interpol
               #:named-readtables
               #:cffi
               #:trivial-macroexpand-all)
  :components ((:file "src/package")
               (:file "src/reader")
               (:file "src/util")
               (:file "src/connection")
               (:file "src/property")
               (:file "src/propspec")
               (:file "src/host")
               (:file "src/combinator")
               (:file "src/deployment")
               (:file "src/connection/local")
               (:file "src/data")
               (:file "src/property/cmd")
               (:file "src/property/file")
               (:file "src/property/os")
               (:file "src/property/service")
               (:file "src/property/apt")
               (:file "src/property/chroot")
               (:file "src/property/user")
               (:file "src/property/git")
               (:file "src/connection/shell-wrap")
	       (:file "src/connection/fork")
               (:file "src/connection/ssh")
               (:file "src/connection/sudo")
               (:file "src/connection/sbcl")
               (:file "src/connection/chroot")
               (:file "src/connection/chroot/fork")
               (:file "src/connection/chroot/shell")
	       (:file "src/connection/setuid")
	       (:file "src/connection/as")
               (:file "src/data/asdf")
               (:file "src/data/pgp")
	       (:file "src/data/git-snapshot")
	       (:file "src/data/gpgpubkeys")))
