(in-package :consfigurator)

(cl:dolist (package '(:consfigurator.core :consfigurator.util))
  (cl:use-package package)
  (cl:do-external-symbols (sym package)
    (cl:export sym)))
