;;; Consfigurator -- Lisp declarative configuration management system

;;; Copyright (C) 2022  David Bremner <david@tethera.net>

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

(in-package :consfigurator/tests)
(named-readtables:in-readtable :consfigurator)

(defparameter *test-gnupg-fingerprint* nil
  "Fingerprint of trusted gpg key usable for encryption and signing.")

(defun first-gpg-fingerprint ()
  "Return the fingerprint of the first (primary) key listed by gpg.

This is mainly useful when there is a single primary key."
  (some
   (lambda (line) (aand (strip-prefix "fpr:::::::::" line)
                        (string-trim ":" it)))
   (lines (gpg '("--with-colons" "--list-keys")))))

(defun make-test-gnupghome ()
  "Create and populate *DATA-SOURCE-GNUPGHOME* for tests."
  (unless (nth-value 1 (ensure-directories-exist
                        *data-source-gnupghome* :mode #o700))
    (error "~s already exists" *data-source-gnupghome*))
  (gpg '("--batch" "--pinentry-mode" "loopback" "--passphrase" "" "--yes"
         "--quick-generate-key" "consfig@example.org (insecure!)"))
  (with-open-file (stream #?"${*data-source-gnupghome*}/gpg.conf"
                          :direction :output)
    (format stream "default-key ~a~%default-recipient-self~%"
            *test-gnupg-fingerprint*)))

(defmacro with-test-gnupg-home (base-dir &rest body)
  "Set up gnupg homedir for test suite under BASE-DIR and run BODY with
*DATA-SOURCE-GNUPGHOME* and *TEST-GNUPG-FINGERPRINT* set appropriately."
  `(let ((*data-source-gnupghome* (merge-pathnames #P"gnupg/" ,base-dir)))
     (unwind-protect
          (progn
            (make-test-gnupghome)
            (let ((*test-gnupg-fingerprint* (first-gpg-fingerprint)))
              ,@body))
       (run-program "gpgconf" "--homedir" *data-source-gnupghome*
                    "--kill" "all"))))

(defparameter *test-pgp-file* nil)

(defmacro with-test-pgp-source (base-dir &rest body)
  "Run BODY with *TEST-PGP-FILE* defined and a corresponding pgp data source
registered and populated."
  `(let ((*test-pgp-file* (merge-pathnames "pgp-secrets.gpg" ,base-dir)))
     (populate-data-pgp)
     (handler-case
         (try-register-data-source :pgp :location *test-pgp-file*)
       (missing-data-source ()
         (error "Test setup failure for pgp file ~a" *test-pgp-file*)))
     ,@body))

(defparameter *test-pass-dir* nil
  "pass(1) store for use in test suite.")

(defun pass (args &key input)
  (run-program `("env" ,#?"GNUPGHOME=${*data-source-gnupghome*}"
                       ,#?"PASSWORD_STORE_DIR=${*test-pass-dir*}" "pass"
                       ,@args)
               :input (if input (make-string-input-stream input) nil)
               :output :string :error-output :output))

(defmacro with-test-pass-source (test-home &rest body)
  "Run BODY with pass(1) data source in TEST-HOME populated and registed."
  `(let ((*test-pass-dir* (merge-pathnames #P"password-store/" ,test-home)))
     (pass (list "init" *test-gnupg-fingerprint*))
     (populate-data-pass)
     (handler-case
         (try-register-data-source :pass :location *test-pass-dir*)
       (missing-data-source ()
         (error "Test setup failure for pass directory ~a" *test-pass-dir*)))
     ,@body))

(defun runner ()
  "Run tests via (sb-)rt, with setup and teardown."
  (with-local-temporary-directory (test-home)
    (with-test-gnupg-home test-home
      (with-reset-data-sources
        (with-test-pgp-source test-home
          (with-test-pass-source test-home
            (do-tests)))))))


;;;; tests for test runner machinery
(deftest runner.0 (not *data-source-gnupghome*) nil)

(deftest runner.1
    (count-if
     (lambda (line) (string-prefix-p "pub" line))
     (lines (gpg '("--with-colons" "--list-keys"))))
  1)

(deftest runner.2 (not *test-gnupg-fingerprint*) nil)

(deftest runner.3 (not *test-pgp-file*) nil)

(deftest runner.4 (nth-value 2 (pass '("list"))) 0)
