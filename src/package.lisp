(in-package :cl-user)

(defpackage :consfigurator
  (:use #:cl #:alexandria)
  (:local-nicknames (#:re #:cl-ppcre))
  (:shadowing-import-from #:uiop
			  #:strcat
			  #:string-prefix-p
			  #:split-string
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
			  #:file-exists-p)
  (:export ;; re-export from UIOP
	   #:strcat
	   #:string-prefix-p
	   #:split-string
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

	   ;; util.lisp
	   #:lines
	   #:unlines
	   #:noop
	   #:symbol-named

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
	   #:get-hostattrs
	   #:push-hostattrs
	   #:get-hostname
	   #:require-data

	   ;; propspec.lisp
	   #:in-consfig
	   #:continue-without-system

	   ;; host.lisp
	   #:defhost

	   ;; deployment.lisp
	   #:defdeploy
	   #:defdeploy-these
	   #:defhostdeploy
	   #:deploy
	   #:deploy-these
	   #:deploys
	   #:deploys-these
	   #:deploy*
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
  (:use #:cl #:consfigurator #:alexandria #:cffi)
  (:export #:local-connection))

(defpackage :consfigurator.connection.debian-sbcl
  (:use #:cl #:consfigurator))

(defpackage :consfigurator.connection.chroot
  (:use #:cl #:consfigurator #:cffi))

(defpackage :consfigurator.connection.chroot.fork
  (:use #:cl #:consfigurator #:cffi))

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
	   #:regex-replace-lines))

(defpackage :consfigurator.data.asdf
  (:use #:cl #:consfigurator))

(defpackage :consfigurator.data.pgp
  (:use #:cl #:consfigurator #:alexandria)
  (:export #:list-data #:get-data #:set-data #:set-data-from-file))
