;;; Consfigurator -- Lisp declarative configuration management system

;;; Copyright (C) 2020-2021  Sean Whitton <spwhitton@spwhitton.name>

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

(in-package :consfigurator.property.gnupg)
(named-readtables:in-readtable :consfigurator)

(defprop public-key-imported :posix (fingerprint)
  "Import the PGP public key identified by FINGERPRINT to gpg's default
keyring."
  (:desc #?"PGP public key ${fingerprint} imported")
  (:preprocess
   (list (remove #\Space fingerprint)))
  (:hostattrs
   (require-data "--pgp-pubkey" fingerprint))
  (:apply
   ;; always do an import, in case we have a newer version of the key than
   ;; last time
   (with-change-if-changes-file (".gnupg/pubring.kbx")
     (mrun
      :input (get-data-stream "--pgp-pubkey" fingerprint) "gpg" "--import"))))
