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

(in-package :consfigurator.property.ccache)
(named-readtables:in-readtable :consfigurator)

(defproplist installed :posix ()
  (:desc "ccache installed")
  (os:etypecase
    (debianlike (apt:installed "ccache"))))

(defprop has-limits :posix (cache
                            &key (max-size nil max-size-supplied-p)
                            (max-files nil max-files-supplied-p))
  "Set limits on a given ccache.
See ccache(1) for the format of MAX-SIZE."
  (:desc (format nil "~A has max size ~D & max files ~D"
                 cache max-size max-files))
  (:apply
   (installed)
   (with-change-if-changes-file-content
       ((merge-pathnames "ccache.conf" (ensure-directory-pathname cache)))
     ;; Let ccache(1) handle editing and deduplicating the config file, etc.
     (mrun "ccache" :env `(:CCACHE_DIR ,cache)
           (and max-size-supplied-p
                (strcat "--max-size=" (or max-size "0")))
           (and max-files-supplied-p
                (strcat "--max-files=" (or max-files "0")))))))

(defpropspec group-cache :posix
    (group &key (max-size nil max-size-supplied-p)
           (max-files nil max-files-supplied-p)
           &aux (dir (ensure-directory-pathname
                      (strcat "/var/cache/ccache-" group))))
  "Configures a ccache in /var/cache for a group."
  (:desc #?"ccache for group ${group} exists")
  `(with-unapply
     (installed)
     (file:directory-exists ,dir)
     (file:has-mode ,dir #o2775)
     (file:has-ownership ,dir :user "root" :group ,group)
     ,@(and (or max-size-supplied-p max-files-supplied-p)
            `((has-limits ,dir
                          ,@(and max-size-supplied-p
                                 `(:max-size ,max-size))
                          ,@(and max-files-supplied-p
                                 `(:max-files ,max-files)))))
     :unapply (file:directory-does-not-exist ,dir)))
