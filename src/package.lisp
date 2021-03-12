(in-package :cl-user)

(defpackage :consfigurator
  (:use #:cl #:alexandria)
  (:local-nicknames (#:re #:cl-ppcre))
  (:shadowing-import-from #:uiop
			  #:strcat
			  #:string-prefix-p
			  #:split-string
			  #:last-char
			  #:escape-sh-command
			  #:escape-sh-token
			  #:run-program
			  #:read-file-string
			  #:copy-stream-to-stream
			  #:subprocess-error
			  #:stripln
			  #:unix-namestring
			  #:pathname-directory-pathname
			  #:with-temporary-file
			  #:ensure-directory-pathname
			  #:getenv
			  #:subdirectories
			  #:directory-files
			  #:file-exists-p
			  #:with-current-directory)
  (:export ;; re-export from UIOP
	   #:strcat
	   #:string-prefix-p
	   #:split-string
	   #:last-char
	   #:escape-sh-command
	   #:escape-sh-token
	   #:run-program
	   #:read-file-string
	   #:copy-stream-to-stream
	   #:subprocess-error
	   #:stripln
	   #:unix-namestring
	   #:pathname-directory-pathname
	   #:with-temporary-file
	   #:ensure-directory-pathname
	   #:getenv
	   #:subdirectories
	   #:directory-files
	   #:file-exists-p
	   #:with-current-directory

	   ;; util.lisp
	   #:lines
	   #:unlines
	   #:noop
	   #:symbol-named
	   #:memstring=

	   #:version<
	   #:version>
	   #:version<=
	   #:version>=

	   #:string->filename
	   #:filename->string

	   ;; connection.lisp
	   #:establish-connection
	   #:preprocess-connection-args
	   #:connection
	   #:lisp-connection
	   #:posix-connection
	   #:lisp-connection-p
	   #:connection-run
	   #:connection-readfile
	   #:connection-writefile
	   #:connection-upload
	   #:connection-teardown

	   #:run
	   #:mrun
	   #:with-remote-temporary-file
	   #:run-failed
	   #:runlines
	   #:test
	   #:readfile
	   #:writefile

	   ;; property.lisp
	   #:propattrs
	   #:propunapply
	   #:defprop
	   #:defproplist
	   #:inapplicable-property
	   #:get-hostattrs
	   #:get-hostattrs-car
	   #:push-hostattrs
	   #:pushnew-hostattrs
	   #:get-hostname
	   #:require-data
	   #:failed-change
           #:assert-euid-root
	   #:assert-connection-supports
	   #:call-with-os

	   ;; propspec.lisp
	   #:in-consfig
	   #:make-propspec
	   #:continue-without-system

	   ;; host.lisp
	   #:host
	   #:defhost
	   #:make-host
	   #:hostattrs

	   ;; deployment.lisp
	   #:defdeploy
	   #:defdeploy-these
	   #:deploy
	   #:deploy*
	   #:deploys
	   #:deploy-these
	   #:deploy-these*
	   #:deploys-these
	   #:continue-deploy*

	   ;; data.lisp
	   #:data
	   #:iden1
	   #:iden2
	   #:data-version
	   #:data-mime
	   #:string-data
	   #:data-string
	   #:file-data
	   #:data-file
	   #:missing-data-source

	   #:try-register-data-source
	   #:register-data-source
	   #:reset-data-sources
	   #:skip-data-source
	   #:get-data-stream
	   #:with-data-stream
	   #:get-data-string
	   #:upload-all-prerequisite-data
	   #:request-lisp-systems
	   #:continue-deploy*-program))

(defpackage :consfigurator.connection.shell-wrap
  (:use #:cl #:consfigurator)
  (:export #:shell-wrap-connection #:connection-shell-wrap))

(defpackage :consfigurator.connection.ssh
  (:use #:cl
	#:consfigurator
	#:alexandria
	#:consfigurator.connection.shell-wrap))

(defpackage :consfigurator.connection.sudo
  (:use #:cl
	#:consfigurator
	#:alexandria
	#:consfigurator.connection.shell-wrap))

(defpackage :consfigurator.connection.local
  (:use #:cl #:consfigurator #:alexandria)
  (:export #:local-connection))

(defpackage :consfigurator.connection.debian-sbcl
  (:use #:cl #:consfigurator))

(defpackage :consfigurator.connection.chroot
  (:use #:cl #:consfigurator #:cffi))

(defpackage :consfigurator.connection.chroot.fork
  (:use #:cl #:consfigurator #-(or sbcl) #:cffi))

(defpackage :consfigurator.connection.chroot.shell
  (:use #:cl
	#:consfigurator
	#:consfigurator.connection.shell-wrap))

(defpackage :consfigurator.property.cmd
  (:use #:cl #:consfigurator)
  (:export #:single))

(defpackage :consfigurator.property.file
  (:use #:cl #:consfigurator #:alexandria)
  (:local-nicknames (#:re #:cl-ppcre))
  (:export #:has-content
	   #:contains-lines
	   #:data-uploaded
	   #:host-data-uploaded
	   #:secret-uploaded
	   #:host-secret-uploaded
	   #:regex-replaced-lines
	   #:directory-exists))

(defpackage :consfigurator.property.os
  (:use #:cl #:consfigurator)
  (:export #:unixlike
	   #:linux
	   #:debianlike
	   #:debian
	   #:debian-stable
	   #:debian-testing
	   #:debian-unstable
	   #:debian-suite
	   #:required
	   #:supports-arch-p))

(defpackage :consfigurator.property.apt
  (:use #:cl #:alexandria #:consfigurator)
  (:local-nicknames (#:re  #:cl-ppcre)
		    (#:file  #:consfigurator.property.file)
		    (#:os    #:consfigurator.property.os))
  (:export #:installed
	   #:removed
	   #:mirror
	   #:standard-sources.list))

(defpackage :consfigurator.property.user
  (:use #:cl #:consfigurator)
  (:local-nicknames (#:os  #:consfigurator.property.os))
  (:export #:has-account))

(defpackage :consfigurator.property.chroot
  (:use #:cl #:consfigurator)
  (:local-nicknames (#:apt   #:consfigurator.property.apt)
		    (#:os    #:consfigurator.property.os)
		    (#:file  #:consfigurator.property.file))
  (:export #:os-bootstrapped))

(defpackage :consfigurator.data.asdf
  (:use #:cl #:consfigurator))

(defpackage :consfigurator.data.pgp
  (:use #:cl #:consfigurator #:alexandria)
  (:export #:list-data #:get-data #:set-data #:set-data-from-file))
