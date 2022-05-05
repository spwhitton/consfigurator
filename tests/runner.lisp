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

(defun runner ()
  "Run tests via (sb-)rt, with setup and teardown."
  (with-local-temporary-directory (test-home)
    (with-test-gnupg-home test-home
      (do-tests))))

;;;; tests for test runner machinery
(deftest runner.0 (not *data-source-gnupghome*) nil)

(deftest runner.1
    (count-if
     (lambda (line) (string-prefix-p "pub" line))
     (lines (gpg '("--with-colons" "--list-keys"))))
  1)

(deftest runner.2 (not *test-gnupg-fingerprint*) nil)
