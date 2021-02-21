(in-package :cl-user)

(defpackage :consfigurator
  (:use #:cl #:alexandria)
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
	   #:connection
	   #:lisp-connection
	   #:posix-connection
	   #:connection-run
	   #:connection-readfile
	   #:connection-writefile
	   #:connection-upload
	   #:connection-teardown

	   #:run
	   #:connection-run-failed
	   #:runlines
	   #:readfile
	   #:writefile

	   ;; property.lisp
	   #:propattrs
	   #:propunapply
	   #:defprop
	   #:get-hostattrs
	   #:push-hostattrs
	   #:get-hostname
	   #:require-data

	   ;; propspec.lisp
	   #:in-consfig

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

	   #:try-register-data-source
	   #:register-data-source
	   #:reset-data-sources
	   #:skip-data-source
	   #:get-data-stream
	   #:with-data-stream
	   #:get-data-string
	   #:upload-all-prerequisite-data))

(defpackage :consfigurator.connection.ssh
  (:use #:cl #:consfigurator))

(defpackage :consfigurator.connection.local
  (:use #:cl #:consfigurator #:alexandria)
  (:export #:local-connection))

(defpackage :consfigurator.connection.debian-sbcl
  (:use #:cl #:consfigurator))

(defpackage :consfigurator.property.file
  (:use #:cl #:consfigurator)
  (:export #:has-content
	   #:contains-lines
	   #:data-uploaded
	   #:host-data-uploaded))

(defpackage :consfigurator.data.asdf
  (:use #:cl #:consfigurator))

(defpackage :consfigurator.data.pgp
  (:use #:cl #:consfigurator #:alexandria)
  (:export #:list-data #:get-data #:set-data #:set-data-from-file))

(defpackage :consfigurator.property.command
  (:use #:cl #:consfigurator)
  (:export #:shell-command))
