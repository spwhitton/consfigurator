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

(in-package :consfigurator.data.gpgpubkeys)
(named-readtables:in-readtable :consfigurator)

(defmethod register-data-source
    ((type (eql :gpgpubkeys)) &key keyring try-recv-key)
  "Obtain ASCII-armoured PGP public keys by querying local gpg keyring KEYRING.
If TRY-RECV-KEY, try to add any missing keys to KEYRING by querying keyservers
configured in dirmngr.conf."
  (unless (file-exists-p keyring)
    (missing-data-source "~A does not exist." keyring))
  (let (cache lastmod)
    (labels ((reset-cache (&optional (new-lastmod (file-write-date keyring)))
               (setq cache (make-hash-table :test #'equal)
                     lastmod new-lastmod))
             (retrieve (iden1 fingerprint)
               (declare (ignore iden1))
               (let ((new-lastmod (file-write-date keyring)))
                 (when (> new-lastmod lastmod)
                   (reset-cache new-lastmod)))
               (or (gethash fingerprint cache)
                   (multiple-value-bind (key queriedp)
                       (getkey keyring fingerprint try-recv-key)
                     (when queriedp (reset-cache))
                     (and key
                          (setf (gethash fingerprint cache)
                                (make-instance 'string-data
                                               :string key
                                               :mime "application/pgp-keys"
                                               :iden1 "--pgp-pubkey"
                                               :iden2 fingerprint
                                               :version lastmod))))))
             (check (iden1 iden2)
               ;; We can't avoid running gpg(1) to find out whether a PGP key
               ;; is available, so we might as well just do the extraction
               ;; work when asked whether we can extract.
               (and (string= iden1 "--pgp-pubkey")
                    (let ((retrieved (retrieve nil iden2)))
                      (and retrieved (data-version retrieved))))))
      (reset-cache)
      (cons #'check #'retrieve))))

(defun local-getkey (keyring fingerprint)
  (multiple-value-bind (output _ exit-code)
      (run-program `("gpg" "--armor" "--no-default-keyring"
                           "--keyring" ,(namestring keyring)
                           "--export-options" "export-minimal"
                           "--export" ,fingerprint)
                   :output :string :ignore-error-status t)
    (declare (ignore _))
    (let ((key (stripln output)))
      (and (zerop exit-code) (not (string-equal "" key)) key))))

(defun getkey (keyring fingerprint try-recv-key)
  (let ((local (local-getkey keyring fingerprint)))
    (when (or local (not try-recv-key))
      (return-from getkey (values local nil)))
    (let ((exit-code
            (nth-value
             2 (run-program
                `("gpg" "--no-default-keyring"
                        "--keyring" ,(namestring keyring)
                        "--recv-key" ,fingerprint)
                :output :string :ignore-error-status t))))
      (when (zerop exit-code)
        (values (local-getkey keyring fingerprint) t)))))
