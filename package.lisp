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
	   #:defprop
	   #:defhost))

(defpackage :consfigurator)

(defpackage :consfigurator.connection.ssh
  (:use #:cl #:consfigurator))

(defpackage :consfigurator.property.file
  (:use #:cl #:consfigurator)
  (:export #:file-has-content
	   #:file-contains-lines))

(defpackage :consfigurator.property.command
  (:use #:cl #:consfigurator)
  (:export #:shell-command))
