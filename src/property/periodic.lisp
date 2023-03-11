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

(in-package :consfigurator.property.periodic)
(named-readtables:in-readtable :consfigurator)

;; Use of this combinator requires always supplying a description, to reduce
;; the chance of accidental description clashes.
(defmacro at-most (period desc &rest propapps)
  "Only attempt to apply PROPAPPS at most every PERIOD.  Supported values for
PERIOD are :each-reboot, :hourly, :daily, :weekly, :monthly, :yearly.  It is
assumed that a month has 30 days and a year has 365.25 days.

The purpose of this combinator is to avoid applying properties that are
expensive to apply more often than it is useful to apply them.  It is not for
scheduling tasks to occur at specific times or on specific days.

The application of PROPAPPS is tracked by creating a flagfile on the remote
with a name computed from DESC.  The mtime of this file is examined to
determine whether PERIOD has passed and another attempt to apply PROPAPPS
should be made.  Thus, you must ensure that DESC is unique among the
descriptions of all the properties that will be applied to this host as this
user."
  `(at-most* ,period ,desc
             ,(if (cdr propapps) `(eseqprops ,@propapps) (car propapps))))

(define-function-property-combinator at-most* (period desc propapp)
  (symbol-macrolet
      ((flagfile (merge-pathnames
                  (string-to-filename desc)
                  (merge-pathnames "at-most/"
                                   (get-connattr :consfigurator-cache)))))
    (destructuring-bind (psym . args) propapp
      (:retprop :type (propapp-type propapp)
                :desc (lambda-ignoring-args desc)
                :hostattrs (get psym 'hostattrs)
                :check
                (lambda-ignoring-args
                  (let ((now (get-universal-time))
                        (mtime (nth-value 2 (remote-file-stats flagfile))))
                    (and
                     mtime
                     (case period
                       (:each-reboot (< (remote-last-reboot) mtime))
                       (:hourly (< now (+ #.(* 60 60) mtime)))
                       (:daily (< now (+ #.(* 24 60 60) mtime)))
                       (:weekly (< now (+ #.(* 7 24 60 60) mtime)))
                       (:monthly (< now (+ #.(* 30 24 60 60) mtime)))
                       (:yearly
                        (< now (+ #.(ceiling (* 365.25 24 60 60)) mtime)))))))
                :apply (lambda-ignoring-args
                         (prog1 (apply-propapp propapp)
                           (file:containing-directory-exists flagfile)
                           (mrun "touch" flagfile)))
                :args args))))
