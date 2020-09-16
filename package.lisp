(in-package :cl-user)

(defpackage :consfigurator.util
  (:use :cl))

(defpackage :consfigurator.core
  (:use :cl :consfigurator.util))

(defpackage :consfigurator.connection.ssh
  (:use :cl :consfigurator.util :consfigurator.core))

(defpackage :consfigurator.property.file
  (:use #:cl #:consfigurator.util #:consfigurator.core)
  (:export #:file-has-content
	   #:file-contains-lines))

(defpackage :consfigurator.property.command
  (:use #:cl #:consfigurator.util #:consfigurator.core)
  (:export #:shell-command))
