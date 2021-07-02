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

(in-package :consfigurator.property.reboot)
(named-readtables:in-readtable :consfigurator)

(defprop %rebooted-at-end :posix ()
  (:apply (at-end
           (lambda (result)
             (declare (ignore result))
             (mrun "shutdown" "-r" "+1")
             (inform t "*** SYSTEM REBOOT SCHEDULED, one minute delay ***")))))

(defproplist rebooted-at-end :posix ()
  "Schedule a reboot for the end of the current (sub)deployment.
The reboot is scheduled with a one minute delay to allow remote Lisp images to
return correct exit statuses to the root Lisp, for the root Lisp to have time
to download their output, etc."
  (container:when-contained (:reboot) (%rebooted-at-end)))
