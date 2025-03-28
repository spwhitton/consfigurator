;;; Consfigurator -- Lisp declarative configuration management system

;;; Copyright (C) 2015, 2021  Sean Whitton <spwhitton@spwhitton.name>

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

(in-package :consfigurator.property.locale)
(named-readtables:in-readtable :consfigurator)

(defprop %available :posix (locale)
  (:desc (declare (ignore locale)) "/etc/locale.gen updated")
  (:apply
   (assert-remote-euid-root)
   (file:map-remote-file-lines
    "/etc/locale.gen"
    (lambda (lines)
      (loop with found
            for line in lines
            for start = (re:scan #?/\Q${locale}\E\b/ line)
            if start
              collect (subseq line start) and do (setq found t)
            else
              collect line
            finally (unless found
                      (failed-change
                       #?"${locale} not found in /etc/locale.gen")))))))

(defproplist available :posix (locale)
  "Ensure that the locale LOCALE is generated and available.

Fails if a locale is not available to be generated.  That is, a commented out
entry for the locale and an accompanying charset must be present in
/etc/locale.gen.

Per Debian bug #684134 we cannot ensure a locale is generated by means of
APT:RECONFIGURE.  So this property edits /etc/locale.gen manually."
  (:desc #?"${locale} locale available")
  (apt:installed "locales")
  (on-change (%available locale)
    (cmd:single "locale-gen")))

(defprop selected-for :posix (locale &rest locale-variables)
  "Select a locale for a list of global locale variables.

A locale variable is of the form LC_FOO, LANG or LANGUAGE.  See locale(5).
One might say

    (locale:selected-for \"en_GB.UTF-8\" \"LC_PAPER\" \"LC_MONETARY\")

to select the British English locale for paper size and currency conventions.

Note that reverting this property does not make a locale unavailable.  That's
because it might be required for other applications of this property."
  (:desc (format nil "~A locale selected for ~{~A~^ ~}"
                 locale locale-variables))
  (:hostattrs
   (declare (ignore locale locale-variables))
   (os:required 'os:debianlike))
  (:apply
   (ignoring-hostattrs (available locale))
   ;; From trixie /etc/locale.conf is the file to edit, but
   ;; /etc/default/locale is a symlink there, so continue to use the latter,
   ;; for compatibility.
   (with-change-if-changes-file-content ("/etc/default/locale")
     (mrun "update-locale"
           (mapcar (lambda (v) (strcat v "=" locale)) locale-variables))))
  (:unapply
   (declare (ignore locale))
   (with-change-if-changes-file-content ("/etc/default/locale")
     (mrun "update-locale" locale-variables))))
