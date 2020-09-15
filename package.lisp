(in-package :cl-user)

(defpackage :consfigurator.util
  (:use :cl))

(defpackage :consfigurator.core
  (:use :cl :consfigurator.util))

(defpackage :consfigurator.connection.ssh
  (:use :cl :consfigurator.util :consfigurator.core))
