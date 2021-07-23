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

(in-package :consfigurator.property.postfix)
(named-readtables:in-readtable :consfigurator)

(defproplist installed :posix ()
  (:desc "Postfix installed")
  (os:etypecase
    (debianlike (apt:installed "postfix"))))

(defproplist reloaded :posix ()
  (:desc "Postfix reloaded")
  (service:reloaded "postfix"))

(defprop main-configured :posix (&rest pairs)
  "Set key--value pairs in /etc/postfix/main.cf."
  (:desc (format nil "Postfix main.cf configured ~{~A=~A~^, ~}" pairs))
  (:apply
   (if (eql :no-change
            (apply #'file:contains-conf-equals "/etc/postfix/main.cf" pairs))
       :no-change
       (reloaded))))

(define-function-property-combinator mapped-file
    (propapp &optional (file (car (propappargs propapp))))
  "Apply PROPAPP, and if it makes a change, run postmap(1) on FILE, which
defaults to the first argument to PROPAPP."
  (:retprop :type (propapptype propapp)
            :desc (get (car propapp) 'desc)
            :check (get (car propapp) 'check)
            :hostattrs (get (car propapp) 'hostattrs)
            :apply (lambda (&rest args)
                     (when-let ((f (get (car propapp) 'papply)))
                       (if (eql :no-change (apply f args))
                           :no-change
                           (mrun "postmap" file))))
            :unapply
            (lambda (&rest args)
              (when-let ((f (get (car propapp) 'punapply)))
                (apply f args))
              (file:does-not-exist (strcat (unix-namestring file) ".db")))
            :args (cdr propapp)))
