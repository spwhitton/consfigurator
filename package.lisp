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
  (:export #:connection-run
	   #:connection-readfile
	   #:connection-writefile
	   #:connection-upload
	   #:establish-connection
	   #:defprop
	   #:defhost
	   #:setconsfig
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

(in-package :consfigurator)
(dolist (package '(:consfigurator.core :consfigurator.util))
  (use-package package)
  (do-external-symbols (sym package)
    (export sym)))
