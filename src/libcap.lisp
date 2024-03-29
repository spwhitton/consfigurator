(in-package :consfigurator.util.posix1e)

(include "sys/capability.h")

(ctype cap_t "cap_t")
(ctype cap_value_t "cap_value_t")

(cenum cap_flag_t
       ((:cap-effective "CAP_EFFECTIVE"))
       ((:cap-permitted "CAP_PERMITTED"))
       ((:cap-inheritable "CAP_INHERITABLE")))
(cenum cap_flag_value_t ((:cap-set "CAP_SET")) ((:cap-clear "CAP_CLEAR")))

(constant (CAP_CHOWN                "CAP_CHOWN"))
(constant (CAP_DAC_OVERRIDE         "CAP_DAC_OVERRIDE"))
(constant (CAP_DAC_READ_SEARCH      "CAP_DAC_READ_SEARCH"))
(constant (CAP_FOWNER               "CAP_FOWNER"))
(constant (CAP_FSETID               "CAP_FSETID"))
(constant (CAP_KILL                 "CAP_KILL"))
(constant (CAP_SETGID               "CAP_SETGID"))
(constant (CAP_SETUID               "CAP_SETUID"))

#+linux
(progn
  (constant (CAP_SETPCAP            "CAP_SETPCAP"))
  (constant (CAP_LINUX_IMMUTABLE    "CAP_LINUX_IMMUTABLE"))
  (constant (CAP_NET_BIND_SERVICE   "CAP_NET_BIND_SERVICE"))
  (constant (CAP_NET_BROADCAST      "CAP_NET_BROADCAST"))
  (constant (CAP_NET_ADMIN          "CAP_NET_ADMIN"))
  (constant (CAP_NET_RAW            "CAP_NET_RAW"))
  (constant (CAP_IPC_LOCK           "CAP_IPC_LOCK"))
  (constant (CAP_IPC_OWNER          "CAP_IPC_OWNER"))
  (constant (CAP_SYS_MODULE         "CAP_SYS_MODULE"))
  (constant (CAP_SYS_RAWIO          "CAP_SYS_RAWIO"))
  (constant (CAP_SYS_CHROOT         "CAP_SYS_CHROOT"))
  (constant (CAP_SYS_PTRACE         "CAP_SYS_PTRACE"))
  (constant (CAP_SYS_PACCT          "CAP_SYS_PACCT"))
  (constant (CAP_SYS_ADMIN          "CAP_SYS_ADMIN"))
  (constant (CAP_SYS_BOOT           "CAP_SYS_BOOT"))
  (constant (CAP_SYS_NICE           "CAP_SYS_NICE"))
  (constant (CAP_SYS_RESOURCE       "CAP_SYS_RESOURCE"))
  (constant (CAP_SYS_TIME           "CAP_SYS_TIME"))
  (constant (CAP_SYS_TTY_CONFIG     "CAP_SYS_TTY_CONFIG"))
  (constant (CAP_MKNOD              "CAP_MKNOD"))
  (constant (CAP_LEASE              "CAP_LEASE"))
  (constant (CAP_AUDIT_WRITE        "CAP_AUDIT_WRITE"))
  (constant (CAP_AUDIT_CONTROL      "CAP_AUDIT_CONTROL"))
  (constant (CAP_SETFCAP            "CAP_SETFCAP"))
  (constant (CAP_MAC_OVERRIDE       "CAP_MAC_OVERRIDE"))
  (constant (CAP_MAC_ADMIN          "CAP_MAC_ADMIN"))
  (constant (CAP_SYSLOG             "CAP_SYSLOG"))
  (constant (CAP_WAKE_ALARM         "CAP_WAKE_ALARM"))
  (constant (CAP_BLOCK_SUSPEND      "CAP_BLOCK_SUSPEND"))
  (constant (CAP_AUDIT_READ         "CAP_AUDIT_READ"))
  (constant (CAP_PERFMON            "CAP_PERFMON"))
  (constant (CAP_BPF                "CAP_BPF"))
  (constant (CAP_CHECKPOINT_RESTORE "CAP_CHECKPOINT_RESTORE")))
