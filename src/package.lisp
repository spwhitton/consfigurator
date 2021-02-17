(in-package :cl-user)

(defpackage :consfigurator.util
  (:use #:cl)
  (:shadowing-import-from #:uiop
			  #:strcat
			  #:string-prefix-p
			  #:split-string)
  (:export #:strcat
	   #:string-prefix-p
	   #:split-string

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
  (:export #:connect-and-apply
	   #:apply-properties
	   #:connection
	   #:lisp-connection
	   #:posix-connection
	   #:connection-run
	   #:run
	   #:run-with-input
	   #:runlines
	   #:runlines-with-input
	   #:connection-readfile
	   #:readfile
	   #:connection-writefile
	   #:writefile
	   #:connection-upload
	   #:connection-teardown
	   #:*host*
	   #:*hostattrs*
	   #:add-hostattr
	   #:require-data
	   #:establish-connection
	   #:defprop
	   #:defhost
	   #:hostattr
	   #:setconsfig
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
