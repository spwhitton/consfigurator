(in-package :consfigurator.util.posix1e)

(include "sys/types.h" "sys/acl.h")

(ctype acl_tag_t "acl_tag_t")
(ctype acl_type_t "acl_type_t")
(ctype acl_entry_t "acl_entry_t")

(constant (ACL_USER "ACL_USER"))
(constant (ACL_GROUP "ACL_GROUP"))
(constant (ACL_TYPE_ACCESS "ACL_TYPE_ACCESS"))
(constant (ACL_TYPE_DEFAULT "ACL_TYPE_DEFAULT"))
(constant (ACL_NEXT_ENTRY "ACL_NEXT_ENTRY"))
(constant (ACL_FIRST_ENTRY "ACL_FIRST_ENTRY"))
