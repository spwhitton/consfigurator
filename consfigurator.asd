(defsystem "consfigurator"
  :description "Lisp declarative configuration management system"
  :version "1.5.3"
  :author "Sean Whitton <spwhitton@spwhitton.name>"
  :licence "GPL-3+"
  :serial t
  :defsystem-depends-on (#:cffi-grovel)
  :depends-on (#:agnostic-lizard
               #:alexandria
               #:anaphora
               #:babel
               #:babel-streams
               #:bordeaux-threads
               #:cffi
               #:cl-heredoc
               #:cl-interpol
               #:cl-ppcre
               #:closer-mop
               #:named-readtables
               #:osicat
               #:parse-number
               (:feature :sbcl (:require #:sb-posix))
               #:trivial-backtrace)
  :components ((:file "src/package")
               (:file "src/reader")
               (:cffi-grovel-file "src/libc")
               (:cffi-grovel-file "src/libacl")
               (:cffi-grovel-file "src/libcap" :if-feature :linux)
               (:file "src/util")
               (:file "src/util/posix1e")
               (:file "src/connection")
               (:file "src/property")
               (:file "src/propspec")
               (:file "src/host")
               (:file "src/combinator")
               (:file "src/deployment")
               (:file "src/connection/local")
               (:file "src/data")
               (:file "src/image")
               (:file "src/property/cmd")
               (:file "src/property/file")
               (:file "src/property/etc-default")
               (:file "src/property/os")
               (:file "src/property/rc.conf")
               (:file "src/property/container")
               (:file "src/property/periodic")
               (:file "src/property/mount")
               (:file "src/property/service")
               (:file "src/property/apt")
               (:file "src/property/pkgng")
               (:file "src/property/package")
               (:file "src/property/chroot")
               (:file "src/property/disk")
               (:file "src/property/fstab")
               (:file "src/property/crypttab")
               (:file "src/property/user")
               (:file "src/util/linux-namespace")
               (:file "src/property/git")
               (:file "src/property/gnupg")
               (:file "src/property/ssh")
               (:file "src/property/sshd")
               (:file "src/property/locale")
               (:file "src/property/reboot")
               (:file "src/property/installer")
               (:file "src/property/grub")
               (:file "src/property/u-boot")
               (:file "src/property/hostname")
               (:file "src/property/network")
               (:file "src/property/libvirt")
               (:file "src/property/ccache")
               (:file "src/property/schroot")
               (:file "src/property/sbuild")
               (:file "src/property/postfix")
               (:file "src/property/cron")
               (:file "src/property/lets-encrypt")
               (:file "src/property/apache")
               (:file "src/property/systemd")
               (:file "src/property/firewalld")
               (:file "src/property/timezone")
               (:file "src/property/swap")
               (:file "src/property/lxc")
               (:file "src/property/postgres")
               (:file "src/connection/shell-wrap")
	       (:file "src/connection/fork")
               (:file "src/connection/rehome")
               (:file "src/connection/ssh")
               (:file "src/connection/sudo")
               (:file "src/connection/su")
               (:file "src/connection/sbcl")
               (:file "src/connection/chroot")
	       (:file "src/connection/setuid")
	       (:file "src/connection/as")
               (:file "src/connection/linux-namespace")
               (:file "src/data/util")
               (:file "src/data/asdf")
               (:file "src/data/pgp")
	       (:file "src/data/git-snapshot")
	       (:file "src/data/gpgpubkeys")
               (:file "src/data/ssh-askpass")
               (:file "src/data/local-file")
               (:file "src/data/pass")
               (:file "src/data/files-tree"))
  :in-order-to ((test-op (test-op "consfigurator/tests"))))

(defsystem "consfigurator/tests"
  :description
  "Tests for Consfigurator, Lisp declarative configuration management system"
  :version "1.5.3"
  :author "Sean Whitton <spwhitton@spwhitton.name>"
  :licence "GPL-3+"
  :serial t
  :depends-on (#:consfigurator
               (:feature :sbcl (:require #:sb-rt))
               (:feature (:not :sbcl) #:rt))
  :components ((:file "tests/package")
               (:file "tests/runner")
               (:file "tests/data/pass")
               (:file "tests/data/pgp")
               (:file "tests/data/util")
               (:file "tests/reader")
               (:file "tests/util")
               (:file "tests/property/file"))
  :perform (test-op (o c) (symbol-call :consfigurator/tests '#:runner)))
