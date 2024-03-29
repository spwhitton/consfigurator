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
;;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

(in-package :consfigurator.connection.local)
(named-readtables:in-readtable :consfigurator)

(defmethod establish-connection ((type (eql :local)) host &key)
  (make-instance 'local-connection))

(defclass local-connection (lisp-connection)
  ()
  (:documentation
   "Applying properties to the machine Lisp is running on, as Lisp's uid."))

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

(defmethod connection-read-file ((connection local-connection) path)
  (read-file-string path))

(defmethod connection-read-and-remove-file ((connection local-connection) path)
  (prog1 (read-file-string path) (delete-file path)))

(defmethod connection-write-file ((connection local-connection)
                                  path
                                  content
                                  mode)
  ;; we cannot use UIOP:WITH-TEMPORARY-FILE etc., because those do not ensure
  ;; the file is only readable by us, and we might be writing a secret key
  (with-remote-temporary-file
      (temp :connection connection
            :directory (pathname-directory-pathname path))
    (nix:chmod temp mode)
    (etypecase content
      (string
       (with-open-file (stream temp :direction :output :if-exists :supersede)
         (write-string content stream)))
      (stream
       (let ((type (stream-element-type content)))
         (with-open-file (stream temp :direction :output
                                      :if-exists :supersede
                                      :element-type type)
           (copy-stream-to-stream content stream :element-type type)))))
    ;; TEMP's pathname will always have a PATHNAME-TYPE which is the random
    ;; string of characters suffixed to make the filename unique.  If PATH
    ;; doesn't have a file extension then the merging behaviour of RENAME-FILE
    ;; will add the random suffix as the file type of the rename destination.
    ;; So we make two new pathnames.
    (flet ((detype-pathname (pn)
             (make-pathname
              :defaults uiop:*nil-pathname* :type :unspecific
              :name (pathname-file pn) :directory (pathname-directory pn))))
      (rename-file-overwriting-target
       (detype-pathname temp) (detype-pathname path)))))

(defmethod connection-connattr
    ((connection local-connection) (k (eql :XDG_CACHE_HOME)))
  (uiop:xdg-cache-home))
