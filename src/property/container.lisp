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

(in-package :consfigurator.property.container)
(named-readtables:in-readtable :consfigurator)

;;;; Container-related hostattrs

;;; The main purpose of these utilities is to permit conditionalising on what
;;; things are contained, to avoid affecting the host system outside the
;;; container in undesirable ways.  For example, avoiding changing the host
;;; system's hostname because the chroot is for a system with a different
;;; hostname (but still updating /etc/hostname inside the chroot).

(defprop contained :posix (&rest contained)
  "Indicate that each of CONTAINED, a list of symbols, is isolated from the host
system of this container, and so may be changed without fear of affecting the
outside host.

Also implicitly marks this host as a container, such that property combinators
which care about what's contained will not assume that they're running outside
of any container.

This property is usually set by properties which establish containers, like
CHROOT:OS-BOOTSTRAPPED, rather than being added to DEFHOST forms."
  (:desc (format nil "~{~(~S~)~^, ~} ~:*~1{~#[are~;is~:;are~]~} contained"
                 contained))
  (:hostattrs (push-hostattrs 'iscontained contained)))

(defun contained-p (&rest contained)
  "Return non-nil if we are outside of any container, or when each of CONTAINED,
a list of symbols, is contained by this container type."
  (alet (get-hostattrs 'iscontained)
    (or (not it) (loop for factor in contained always (member factor it)))))

(defmacro when-contained ((&rest contained) &body propapps)
  "Macro property combinator.  Apply each of PROPAPPS only when outside of any
container, or when each of CONTAINED, a list of symbols, is contained by this
container type."
  `(when-contained*
    ',contained
    ,(if (cdr propapps) `(eseqprops ,@propapps) (car propapps))))

(define-function-property-combinator when-contained* (contained propapp)
  (macrolet ((check-contained (form)
               `(if (apply #'contained-p contained) ,form :no-change)))
    (:retprop :type (propapp-type propapp)
              :hostattrs (lambda-ignoring-args
                           (propapp-attrs propapp))
              :apply (lambda-ignoring-args
                       (check-contained (apply-propapp propapp)))
              :unapply (lambda-ignoring-args
                         (check-contained (unapply-propapp propapp)))
              :args (cdr propapp))))
