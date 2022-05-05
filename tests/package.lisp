(in-package :cl-user)

(defpackage :consfigurator/tests
  (:use #:cl #:consfigurator #:consfigurator.data.util #+sbcl :sb-rt #-sbcl :rtest)
  (:local-nicknames (#:file       #:consfigurator.property.file)))
