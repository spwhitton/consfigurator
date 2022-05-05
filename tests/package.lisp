(in-package :cl-user)

(defpackage :consfigurator/tests
  (:use #:cl #:consfigurator #:consfigurator.data.util #:alexandria #:anaphora
   #+sbcl :sb-rt #-sbcl :rtest)
  (:local-nicknames (#:file       #:consfigurator.property.file)
                    (#:data.pgp   #:consfigurator.data.pgp)))
