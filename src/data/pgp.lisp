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

(in-package :consfigurator.data.pgp)
(named-readtables:in-readtable :consfigurator)

;; Simple PGP-encrypted file source of prerequisite data

;; We provide an implementation of REGISTER-DATA-SOURCE and functions for the
;; user to call at the REPL to add pieces of data, see what's there, etc.  (a
;; prerequisite data source which was some sort of external file-generating or
;; secrets storage database might not provide any functions for the REPL).
;;
;; You will need to use SET-DATA to create an encrypted store before
;; attempting to call REGISTER-DATA-SOURCE in your consfig.

(defmethod register-data-source ((type (eql :pgp)) &key location)
  (unless (file-exists-p location)
    (error 'missing-data-source
           :text (format nil "Could not open ~A" location)))
  (let ((mod (file-write-date location))
        (cache (read-store location)))
    (labels ((update-cache ()
               (let ((new-mod (file-write-date location)))
                 (when (> new-mod mod)
                   (setq mod new-mod
                         cache (read-store location)))))
             (check (iden1 iden2)
               (update-cache)
               (cadr (data-assoc iden1 iden2 cache)))
             (extract (iden1 iden2)
               (update-cache)
               (let ((data (data-assoc iden1 iden2 cache)))
                 (make-instance 'string-data
                                :iden1 iden1 :iden2 iden2
                                :string (cddr data) :version (cadr data)))))
      (cons #'check #'extract))))

(defun read-store (location)
  (handler-case
      (read-from-string
       (run-program
        (escape-sh-command (list "gpg" "--decrypt" (unix-namestring location)))
        :output :string))
    (subprocess-error (error)
      (missing-data-source "While attempt to decrypt, gpg exited with ~A"
			   (uiop:subprocess-error-code error)))))

(defun put-store (location data)
  (run-program (list "gpg" "--encrypt")
               :input (make-string-input-stream (prin1-to-string data))
               :output (unix-namestring location)))

(defun data-assoc (iden1 iden2 data)
  (assoc (cons iden1 iden2) data
         :test (lambda (x y)
                 (and (string= (car x) (car y))
                      (string= (cdr x) (cdr y))))))

(defun get-data (location iden1 iden2)
  "Fetch a piece of prerequisite data.

Useful at the REPL."
  (cddr (data-assoc iden1 iden2 (read-store location))))

(defun set-data (location iden1 iden2 val)
  "Set a piece of prerequisite data.

Useful at the REPL."
  (let ((data (delete-if
               (lambda (d)
                 (and (string= (caar d) iden1) (string= (cdar d) iden2)))
               (and (file-exists-p location) (read-store location)))))
    (push (cons (cons iden1 iden2) (cons (get-universal-time) val)) data)
    (put-store location data)))

(defun set-data-from-file (location iden1 iden2 file)
  "Set a piece of prerequisite data from the contents of a file.

Useful at the REPL."
  (set-data location iden1 iden2 (read-file-string file)))

(defun list-data (location)
  "List all prerequisite data in the PGP store at LOCATION.

Useful at the REPL."
  (dolist (item (read-store location))
    (format t "~A ~A~%" (caar item) (cdar item))))
