(in-package :cl-user)

(defpackage :consfigurator.util
  (:use #:cl)
  (:shadowing-import-from #:uiop
			  #:strcat
			  #:string-prefix-p
			  #:split-string
			  #:escape-sh-command
			  #:escape-sh-token
			  #:run-program
			  #:read-file-string
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
  (:export #:strcat
	   #:string-prefix-p
	   #:split-string
	   #:escape-sh-command
	   #:escape-sh-token
	   #:run-program
	   #:read-file-string
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

	   #:lines
	   #:unlines
	   #:noop
	   #:symbol-named

	   #:version<
	   #:version>
	   #:version<=
	   #:version>=

	   #:string->filename
	   #:filename->string))

(defpackage :consfigurator.core
  (:use #:cl
	#:alexandria
	#:consfigurator.util)
  (:export #:establish-connection
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

	   #:propattrs
	   #:propunapply
	   #:defprop
	   #:get-hostattrs
	   #:push-hostattrs
	   #:require-data

	   #:in-consfig
	   #:hostattr
	   #:defhost

	   #:*host*
	   #:defdeploy
	   #:defdeploy-these
	   #:defhostdeploy
	   #:deploy
	   #:deploy-these

	   #:add-data-source
	   #:register-data-source
	   #:data-uploaded
	   #:host-data-uploaded
	   #:get-data
	   #:upload-all-prerequisite-data))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :consfigurator)
    (make-package :consfigurator :use '(cl))))

(defpackage :consfigurator.connection.ssh
  (:use #:cl #:consfigurator))

(defpackage :consfigurator.connection.local
  (:use #:cl #:consfigurator)
  (:export #:local-connection))

(defpackage :consfigurator.connection.debian-sbcl
  (:use #:cl #:consfigurator))

(defpackage :consfigurator.property.file
  (:use #:cl #:consfigurator)
  (:export #:file-has-content
	   #:file-contains-lines))

(defpackage :consfigurator.data.asdf
  (:use #:cl #:consfigurator))

(defpackage :consfigurator.data.pgp
  (:use #:cl #:consfigurator))

(defpackage :consfigurator.property.command
  (:use #:cl #:consfigurator)
  (:export #:shell-command))

(in-package :consfigurator)
(dolist (package '(:consfigurator.core :consfigurator.util))
  (use-package package)
  (do-external-symbols (sym package)
    (export sym)))
