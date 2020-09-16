(in-package :cl-user)

(defpackage :consfigurator.util
  (:use #:cl)
  (:export #:concat
	   #:lines
	   #:unlines
	   #:noop
	   #:shellcmd))

(defpackage :consfigurator.core
  (:use #:cl
	#:alexandria
	#:consfigurator.util)
  (:export #:*connection*
	   #:connection
	   #:connection-run
	   #:connection-readfile
	   #:connection-writefile
	   #:connection-upload
	   #:establish-connection
	   #:defprop))

(defpackage :consfigurator.connection.ssh
  (:use #:cl #:consfigurator.util #:consfigurator.core))

(defpackage :consfigurator.property.file
  (:use #:cl #:consfigurator.util #:consfigurator.core)
  (:export #:file-has-content
	   #:file-contains-lines))

(defpackage :consfigurator.property.command
  (:use #:cl #:consfigurator.util #:consfigurator.core)
  (:export #:shell-command))
