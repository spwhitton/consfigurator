(in-package :consfigurator)

#+linux
(progn
  (define "_GNU_SOURCE")
  (include "linux/sched.h"))

#+linux
(progn
  (constant (+CLONE_NEWNS+ "CLONE_NEWNS")))
