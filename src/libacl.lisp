(in-package :consfigurator.util.posix1e)

(include "sys/types.h" "sys/acl.h")

(ctype acl_tag_t "acl_tag_t")
(ctype acl_type_t "acl_type_t")
(ctype acl_entry_t "acl_entry_t")

(constant (+ACL-USER+ "ACL_USER"))
(constant (+ACL-GROUP+ "ACL_GROUP"))
(constant (+ACL-TYPE-ACCESS+ "ACL_TYPE_ACCESS"))
(constant (+ACL-TYPE-DEFAULT+ "ACL_TYPE_DEFAULT"))
(constant (+ACL-NEXT-ENTRY+ "ACL_NEXT_ENTRY"))
(constant (+ACL-FIRST-ENTRY+ "ACL_FIRST_ENTRY"))
