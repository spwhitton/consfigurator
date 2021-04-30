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

(in-package :consfigurator.property.mount)
(named-readtables:in-readtable :consfigurator)

(defprop unmounted-below :posix (dir)
  "Unmount anything mounted at or below DIR."
  (:desc #?"${dir} unmounted")
  (:hostattrs
   (declare (ignore dir))
   ;; findmnt(1) & arguments to mount(1)/umount(1) we use are from util-linux
   (os:required 'os:linux))
  (:apply
   (with-change-if-changes-file-content ("/proc/mounts")
     (let* ((dir (ensure-directory-pathname dir))
            (all-mounts
              (mapcar #'ensure-directory-pathname
                      (runlines "findmnt" "-rn" "--output" "target")))
            (mounts-below (remove-if-not (rcurry #'subpathp dir) all-mounts))
            (sorted (sort mounts-below #'string< :key #'unix-namestring)))
       (loop as next = (pop sorted)
             while next
             do (loop while (subpathp (car sorted) next) do (pop sorted))
                ;; If any of the mounts were made with --rbind then unmounting
                ;; will unmount the source filesystems too (e.g. things
                ;; mounted under /dev), so use --make-rslave to prevent that.
                (mrun "mount" "--make-rslave" next)
                (mrun "umount" "--recursive" next))))))

(defproplist unmounted-below-and-removed :posix (dir)
  "Unmount anything mounted at or below DIR and recursively delete dir."
  (:desc #?"${dir} unmounted and removed")
  (:check (not (remote-exists-p dir)))
  (unmounted-below dir)
  (cmd:single "rm" "-rf" dir))
