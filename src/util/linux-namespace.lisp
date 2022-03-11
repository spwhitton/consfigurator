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

(in-package :consfigurator.util.linux-namespace)
(named-readtables:in-readtable :consfigurator)

(defun get-ids-offset (file identifier)
  "Where IDENTIFIER is a username or uid, and FILE is structured like
/etc/subuid and /etc/subuid (see subuid(5) and subgid(5)), return the
numerical subordinate ID and numerical subordinate ID count for the first
entry in FILE for IDENTIFIER."
  (with-open-file (file file)
    (loop with info = (osicat:user-info identifier)
          with fields
            = (list (cdr (assoc :name info))
                    (write-to-string (cdr (assoc :user-id info))))
          for line = (read-line file)
          for (field start count) = (split-string line :separator '(#\:))
          when (memstr= field fields)
            return (values (parse-integer start) (parse-integer count)))))

(defun reduce-id-maps (id-maps)
  "Where each of ID-MAPS is a list of three integers corresponding to the lines
of the uid_map (resp. gid_map) of a process in a different user namespace as
would be read by a process in the current user namespace, return a function
which maps UIDs (resp. GIDs) in the current user namespace to UIDs
(resp. GIDs) in the user namespace of the process.  The function returns NIL,
not 65534, for values which are unmapped.

A process's uid_map & gid_map files are under /proc; see user_namespaces(7)."
  (let ((cache (make-hash-table))
        (sorted (sort (copy-list id-maps) #'< :key #'car)))
    (labels ((make-subtree (limit)
               (and (plusp limit)
                    (let* ((new-limit (floor limit 2))
                           (left (make-subtree new-limit))
                           (next (pop sorted)))
                      (destructuring-bind (inside outside count) next
                        (let ((end (1- (+ inside count))))
                          (assert (and (every #'integerp next)
                                       (not (minusp inside))
                                       (not (minusp outside))
                                       (plusp count))
                                  nil "Invalid ID map ~S" next)
                          (assert
                           (or (not sorted) (> (caar sorted) end))
                           nil "ID maps overlap: ~S & ~S" next (car sorted))
                          (list inside end (- outside inside) left
                                (make-subtree (1- (- limit new-limit)))))))))
             (tree-lookup (id tree)
               (and tree
                    (destructuring-bind (start end offset left right) tree
                      (cond ((> id end) (tree-lookup id right))
                            ((< id start) (tree-lookup id left))
                            (t (+ id offset))))))
             ;; Memoisation ought to be really worthwhile because we will
             ;; likely be looking up the same few IDs over and over (e.g. 0).
             (cache-or-tree-lookup (id tree)
               (multiple-value-bind (result found) (gethash id cache)
                 (if found
                     result
                     (setf (gethash id cache) (tree-lookup id tree))))))
      (rcurry #'cache-or-tree-lookup (make-subtree (length sorted))))))

(defun shift-ids (root uidmap gidmap
                  &aux (seen (make-hash-table :test #'equal)))
  "Recursively map the ownership and POSIX ACLs of files under ROOT by applying
the function UIDMAP to user ownership and UIDs appearing in ACLs, and the
function GIDMAP to group ownership and GIDs appearing in ACLs.  Each of UIDMAP
and GIDMAP should return a non-negative integer or NIL for each non-negative
integer input; in the latter case, no update will be made to the UID or GID.

For example, to recursively shift the ownership and POSIX ACLs of a filesystem
hierarchy to render it suitable for use as a root filesystem in a different
user namespace, you might use

    (shift-ids \"/var/lib/lxc/mycontainer/rootfs\"
               (reduce-id-maps '(0 100000 65536))
               (reduce-id-maps '(0 100000 65536)))

Here the list (0 100000 65536) describes the relationship between the present
user namespace and the container's user namespace; see the docstring for
CONSFIGURATOR.UTIL.LINUX-NAMESPACE:REDUCE-ID-MAPS and user_namespaces(7)."
  (labels
      ((shift (file)
         (let* ((file (drop-trailing-slash (unix-namestring file)))
                (stat (nix:lstat file))
                (pair (cons (nix:stat-dev stat) (nix:stat-ino stat)))
                (uid (nix:stat-uid stat))
                (gid (nix:stat-gid stat))
                (mode (nix:stat-mode stat))
                (dirp (nix:s-isdir mode))
                (linkp (nix:s-islnk mode)))
           (unless (gethash pair seen)
             (setf (gethash pair seen) t)
             (nix:lchown file
                         (or (funcall uidmap uid) uid)
                         (or (funcall gidmap gid) gid))
             (unless linkp
               ;; Restore mode because chown wipes setuid/setgid.
               (nix:chmod file mode)
               ;; Now do the ACL shifts; directories have two.
               (shift-acl file +ACL-TYPE-ACCESS+)
               (when dirp (shift-acl file +ACL-TYPE-DEFAULT+)))
             (when (and dirp (not linkp))
               (mapc #'shift (local-directory-contents file))))))
       (shift-acl (file type)
         (with-acl-free ((acl (acl-get-file file type)))
           (with-foreign-objects
               ((uid 'uid_t) (gid 'gid_t) (entry-p 'acl_entry_t))
             (loop with setp
                   for etype = +ACL-FIRST-ENTRY+ then +ACL-NEXT-ENTRY+
                   while (plusp (acl-get-entry acl etype entry-p))
                   for entry = (mem-ref entry-p 'acl_entry_t)
                   for tag-type = (acl-get-tag-type entry)
                   when (= tag-type +ACL-USER+)
                     do (awhen
                            (funcall uidmap (acl-get-qualifier entry 'uid_t))
                          (setf setp t (mem-ref uid 'uid_t) it)
                          (acl-set-qualifier entry uid))
                   when (= tag-type +ACL-GROUP+)
                     do (awhen
                            (funcall gidmap (acl-get-qualifier entry 'gid_t))
                          (setf setp t (mem-ref gid 'gid_t) it)
                          (acl-set-qualifier entry gid))
                   finally (when setp (acl-set-file file type acl)))))))
    (shift (ensure-directory-pathname root))))

#+linux
(defun get-userns-owner (fd)
  (with-foreign-object (owner 'uid_t)
    (if (minusp
         (foreign-funcall
          "ioctl" :int fd :unsigned-long +NS_GET_OWNER_UID+ :pointer owner
                  :int))
        (error "Couldn't determine owner of target userns.")
        (mem-ref owner 'uid_t))))

(defun setgroups-p ()
  "In a Lisp-type connection, do we have the ability to use setgroups(2)?"
  (and #-linux (zerop (nix:geteuid))
       #+linux (capability-p :cap-effective +CAP-SETGID+)
       #+linux (string= "allow"
                        (stripln
                         (read-file-string "/proc/thread-self/setgroups")))))
