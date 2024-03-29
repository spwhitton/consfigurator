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
;;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

(in-package :consfigurator.property.timezone)
(named-readtables:in-readtable :consfigurator)

(defproplist configured :posix (timezone)
  "Set the system timezone.  TIMEZONE is a relative path under /usr/share/zoneinfo,
e.g. \"Europe/London\"."
  (:hostattrs (push-hostattr 'timezone timezone))
  (os:etypecase
    (linux
     (file:symlinked :from "/etc/localtime"
                     :to (merge-pathnames timezone #P"/usr/share/zoneinfo/"))))
  (os:typecase
    (os:debianlike
     (on-change (file:has-content "/etc/timezone" (list timezone))
       (apt:reconfigured "tzdata")))))

(defproplist configured-from-parent :posix ()
  "Sets the system timezone to match the parent host's."
  (configured (or (get-parent-hostattrs-car 'timezone)
                  (failed-change "Parent has no known timezone"))))
