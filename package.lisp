(in-package :cl-user)

(defpackage :consfigurator.util
  (:use #:cl)
  (:export #:concat
	   #:lines
	   #:unlines
	   #:noop
	   #:shellcmd
	   #:symbol-named))

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
	   #:connection-readfile
	   #:readfile
	   #:connection-writefile
	   #:writefile
	   #:connection-upload
	   #:connection-teardown
	   #:*host*
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
	   #:get-path-to-concatenated-system))

(defpackage :consfigurator (:use #:cl))

(defpackage :consfigurator.connection.ssh
  (:use #:cl #:consfigurator))

(defpackage :consfigurator.property.file
  (:use #:cl #:consfigurator)
  (:export #:file-has-content
	   #:file-contains-lines))

(defpackage :consfigurator.property.command
  (:use #:cl #:consfigurator)
  (:export #:shell-command))

(defpackage :consfigurator.data.pgp
  (:use #:cl #:consfigurator))

(in-package :consfigurator)
(dolist (package '(:consfigurator.core :consfigurator.util))
  (use-package package)
  (do-external-symbols (sym package)
    (export sym)))
