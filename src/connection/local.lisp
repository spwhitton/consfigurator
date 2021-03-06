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

(in-package :consfigurator.connection.local)

(defmethod establish-connection ((type (eql :local)) host &key)
  (make-instance 'local-connection))

(defclass local-connection (lisp-connection)
  ()
  (:documentation "The root deployment: applying properties to the machine the
root Lisp is running on, as the root Lisp's uid."))

(defmethod connection-run ((c local-connection) cmd (s stream))
  ;; see https://gitlab.common-lisp.net/asdf/asdf/-/issues/59
  (call-next-method c cmd `(,s :element-type ,(stream-element-type s))))

(defmethod connection-run ((c local-connection) cmd (s string))
  (call-next-method c cmd (make-string-input-stream s)))

(defmethod connection-run ((connection local-connection) shell-cmd input)
  (multiple-value-bind (output _ exit-code)
      ;; call sh(1) so we know we'll get POSIX
      (run-program `("sh" "-c" ,shell-cmd)
		   :input input :output :string
		   :error-output :output :ignore-error-status t)
    (declare (ignore _))
    (values output exit-code)))

(defmethod connection-readfile ((connection local-connection) path)
  (read-file-string path))

;; in the following two functions, we cannot use UIOP:WITH-TEMPORARY-FILE
;; etc., because those do not ensure the file is only readable by us, and we
;; might be writing a secret key

(defmethod connection-writefile ((connection local-connection)
				 path
				 (content string)
				 mode)
  (with-remote-temporary-file
      (temp :connection connection
	    :directory (pathname-directory-pathname path))
    (run-program `("chmod" ,(format nil "~O" mode) ,temp))
    (with-open-file (stream temp :direction :output :if-exists :supersede)
      (write-string content stream))
    (run-program `("mv" ,temp ,path))))

(defmethod connection-writefile ((connection local-connection)
				 path
				 (content stream)
				 mode
				 &aux
				   (type (stream-element-type content)))
  (with-remote-temporary-file
      (temp :connection connection
	    :directory (pathname-directory-pathname path))
    (run-program `("chmod" ,(format nil "~O" mode) ,temp))
    (with-open-file (stream temp :direction :output
				 :if-exists :supersede
				 :element-type type)
      (copy-stream-to-stream content stream :element-type type))
    (run-program `("mv" ,temp ,path))))

(defmethod connection-upload ((connection local-connection) from to)
  (copy-file from to))
