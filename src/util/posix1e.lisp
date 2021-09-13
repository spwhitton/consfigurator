;;; Consfigurator -- Lisp declarative configuration management system

;;; Copyright (C) 2021  Sean Whitton <spwhitton@spwhitton.name>

;;; This file is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3, or (at your option)
;;; any later version.

;;; This file is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.

;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(in-package :consfigurator.util.posix1e)
(named-readtables:in-readtable :consfigurator)

(define-foreign-library libacl (t (:default "libacl")))

(use-foreign-library libacl)

(define-error-retval-cfun () "acl_free" :int (obj_p :pointer))

(define-error-retval-cfun (:failure-val (null-pointer))
  "acl_get_file" :pointer (path-p :string) (type acl_type_t))

(define-error-retval-cfun ()
  "acl_set_file" :int (path-p :string) (type acl_type_t) (acl :pointer))

(define-error-retval-cfun ()
  "acl_get_entry" :int (acl :pointer) (entry-id :int) (entry-p :pointer))

(define-error-retval-cfun ()
  ("acl_get_tag_type" %acl-get-tag-type)
  :int (entry-d acl_entry_t) (tag-type-p :pointer))

(defun acl-get-tag-type (entry-d)
  (with-foreign-object (tag-type-p 'acl_tag_t)
    (%acl-get-tag-type entry-d tag-type-p)
    (mem-ref tag-type-p 'acl_tag_t)))

(defmacro with-acl-free ((aclvar aclcall) &body forms)
  (with-gensyms (aclvar*)
    `(let* ((,aclvar ,aclcall)
            (,aclvar* (make-pointer (pointer-address ,aclvar))))
       (unwind-protect (progn ,@forms) (acl-free ,aclvar*)))))

(define-error-retval-cfun (:failure-val (null-pointer))
  ("acl_get_qualifier" %acl-get-qualifier) :pointer (entry-d acl_entry_t))

(define-error-retval-cfun ()
  "acl_set_qualifier" :int (entry-d acl_entry_t) (qualifier-p :pointer))

(defun acl-get-qualifier (entry-d type)
  (with-acl-free (qualifier-p (%acl-get-qualifier entry-d))
    (mem-ref qualifier-p type)))
