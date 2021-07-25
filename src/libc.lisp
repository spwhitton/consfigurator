(in-package :consfigurator)

(include "unistd.h")

(ctype uid_t "uid_t")

#+linux
(progn
  (define "_GNU_SOURCE")
  (include "linux/sched.h")
  (include "linux/capability.h")
  (include "linux/nsfs.h"))

#+linux
(progn
  (constant (+CLONE_NEWCGROUP+ "CLONE_NEWCGROUP"))
  (constant (+CLONE_NEWIPC+    "CLONE_NEWIPC"))
  (constant (+CLONE_NEWNET+    "CLONE_NEWNET"))
  (constant (+CLONE_NEWNS+     "CLONE_NEWNS"))
  (constant (+CLONE_NEWPID+    "CLONE_NEWPID"))
  (constant (+CLONE_NEWTIME+   "CLONE_NEWTIME"))
  (constant (+CLONE_NEWUSER+   "CLONE_NEWUSER"))
  (constant (+CLONE_NEWUTS+    "CLONE_NEWUTS"))

  (constant (+NS_GET_OWNER_UID+ "NS_GET_OWNER_UID")))
