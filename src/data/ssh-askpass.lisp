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

(in-package :consfigurator.data.ssh-askpass)
(named-readtables:in-readtable :consfigurator)

(defmethod register-data-source
    ((type (eql :ssh-askpass)) &key iden1-re iden2-re)
  "Data source which will attempt to provide any piece of data matching the
CL-PPCRE regular expressions IDEN1-RE and IDEN2-RE, obtaining the data by
using ssh-askpass(1) to prompt the user to input it.  Useful for things like
sudo passwords."
  (unless (uiop:getenv "DISPLAY")
    (missing-data-source "DISPLAY not set; cannot launch ssh-askpass(1)."))
  (let ((cache (make-hash-table :test #'equal)))
    (cons
     (lambda (iden1 iden2)
       (and (re:scan iden1-re iden1) (re:scan iden2-re iden2)))
     (lambda (iden1 iden2)
       (let ((pair (cons iden1 iden2)))
         (or (gethash pair cache)
             (setf (gethash pair cache)
                   (loop with msg
                         for first = (ssh-askpass iden1 iden2 msg)
                         for second = (ssh-askpass iden1 iden2 "confirm")
                         if (string= first second)
                           return (make-instance
                                   'string-data
                                   :string first :mime "text/plain"
                                   :version (get-universal-time)
                                   :iden1 iden1 :iden2 iden2)
                         else do (setq msg "did not match; try again")))))))))

(defun ssh-askpass (iden1 iden2 &optional note)
  (stripln (run-program
            (list "ssh-askpass"
                  (format nil "~A | ~A~:[~; (~:*~A)~]" iden1 iden2 note))
            :output :string)))
