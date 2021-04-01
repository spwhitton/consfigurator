(in-package :cl-user)

(defpackage :consfigurator/tests
  (:use #:cl #:consfigurator #+sbcl :sb-rt #-sbcl :rtest)
  (:local-nicknames (#:file       #:consfigurator.property.file)))
