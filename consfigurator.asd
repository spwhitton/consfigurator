(defsystem "consfigurator"
  :description "Lisp declarative configuration management system"
  :version "0.5.0"
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
               #+sbcl #:sb-posix
               #:closer-mop
               #:trivial-backtrace
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
               (:file "src/property/disk")
               (:file "src/property/mount")
               (:file "src/property/service")
               (:file "src/property/apt")
               (:file "src/property/chroot")
               (:file "src/property/live-build")
               (:file "src/property/user")
               (:file "src/property/git")
               (:file "src/property/gnupg")
               (:file "src/property/ssh")
               (:file "src/property/sshd")
               (:file "src/property/locale")
               (:file "src/connection/shell-wrap")
	       (:file "src/connection/fork")
               (:file "src/connection/rehome")
               (:file "src/connection/ssh")
               (:file "src/connection/sudo")
               (:file "src/connection/su")
               (:file "src/connection/sbcl")
               (:file "src/connection/chroot")
               (:file "src/connection/chroot/fork")
               (:file "src/connection/chroot/shell")
	       (:file "src/connection/setuid")
	       (:file "src/connection/as")
               (:file "src/data/asdf")
               (:file "src/data/pgp")
	       (:file "src/data/git-snapshot")
	       (:file "src/data/gpgpubkeys"))
  :in-order-to ((test-op (test-op "consfigurator/tests"))))

(defsystem "consfigurator/tests"
  :description
  "Tests for Consfigurator, Lisp declarative configuration management system"
  :version "0.5.0"
  :author "Sean Whitton <spwhitton@spwhitton.name>"
  :licence "GPL-3+"
  :serial t
  :depends-on (#:consfigurator
               #+sbcl #:sb-rt #-sbcl #:rt)
  :components ((:file "tests/package")
               (:file "tests/property/file"))
  :perform (test-op (o c) (symbol-call :consfigurator/tests '#:do-tests)))
