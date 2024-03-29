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

(in-package :consfigurator.property.gnupg)
(named-readtables:in-readtable :consfigurator)

(defprop %public-key-imported :posix (fingerprint)
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

(defprop %trusts-public-key :posix (fingerprint level)
  (:desc #?"PGP public key ${fingerprint} trusted, level ${level}")
  (:preprocess (list (remove #\Space fingerprint) level))
  (:apply (with-change-if-changes-file (".gnupg/trustdb.gpg")
            (mrun :input (format nil "~A:~A:~%" fingerprint level)
                  "gpg" "--import-ownertrust"))))

(defpropspec public-key-imported :posix (fingerprint &key trust-level)
  "Import the PGP public key identified by FINGERPRINT to gpg's default keyring.
If TRUST-LEVEL, also ensure that the key is trusted at that level, an
integer."
  (:desc
   (if trust-level
       #?"PGP public key ${fingerprint} imported and trusted, level ${trust-level}"
       #?"PGP public key ${fingerprint} imported"))
  (if trust-level
      `(eseqprops (%public-key-imported ,fingerprint)
                  (%trusts-public-key ,fingerprint ,trust-level))
      `(%public-key-imported ,fingerprint)))

(defprop secret-key-imported :posix (fingerprint)
  (:desc #?"PGP secret key ${fingerprint} imported")
  (:preprocess (list (remove #\Space fingerprint)))
  (:hostattrs (require-data "--pgp-seckey" fingerprint))
  (:check
   ;; Look for plain "sec" not, e.g., "sec#", which indicates the secret key
   ;; is not available.
   (multiple-value-bind (out err exit)
       (run :may-fail "gpg" "--list-secret-keys" fingerprint)
     (declare (ignore err))
     (and (zerop exit) (re:scan #?/^sec\s/ out))))
  (:apply (mrun :input (get-data-stream "--pgp-seckey" fingerprint)
                "gpg" "--batch" "--no-tty" "--import")))
