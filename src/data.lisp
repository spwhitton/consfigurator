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

(in-package :consfigurator)
(named-readtables:in-readtable :consfigurator)

;;;; Prerequisite data

(defclass data ()
  ((iden1
    :initarg :iden1
    :reader iden1)
   (iden2
    :initarg :iden2
    :reader iden2)
   (data-version
    :initarg :version
    :reader data-version
    :initform nil)
   (data-mime
    :initarg :mime
    :accessor data-mime
    :initform nil
    :documentation "The MIME type of the data, if known."))
  (:documentation
   "An item of prerequisite data as provided by a registered prerequisite data
source, or, outside of the root Lisp, as fished out of a local cache of
prerequisite data."))

(defclass string-data (data)
  ((data-string
    :initarg :string
    :reader data-string))
  (:documentation
   "An item of prerequisite data directly accessible to Lisp."))

(defclass file-data (data)
  ((data-cksum :initarg :cksum)
   (data-file :initarg :file :reader data-file))
  (:documentation
   "An item of prerequisite data accessible via the filesystem."))

(defgeneric data-cksum (data)
  (:documentation
   "Return a CRC checksum for the data as calculated by POSIX cksum(1).")
  (:method ((data file-data))
    (if (slot-boundp data 'data-cksum)
        (slot-value data 'data-cksum)
        (setf (slot-value data 'data-cksum)
              (parse-integer
               (car
                (split-string
                 (run-program `("cksum" ,(unix-namestring (data-file data)))
                              :output :string))))))))

;; If this proves to be inadequate then an alternative would be to maintain a
;; mapping of ASDF systems to data sources, and then DEPLOY* could look up the
;; data sources registered for the systems in (slot-value (slot-value host
;; 'propspec) 'systems) and bind *data-sources* to point to those just how it
;; binds *host* and *connection*.  Registering a source would the mean
;; registering it in the mapping of systems to sources.
;;
;; Sources of data are not associated with propspecs because the latter might
;; request "/etc/foo/key.sec for whatever host I'm being applied to" using
;; FILE:HOST-DATA-UPLOADED, and different hosts in different consfigs might
;; have different data sources for that file.  So one possibility is to
;; associate sources of prerequisite data to hosts, or to the consfigs which
;; define those hosts.  But associating them to the root Lisp makes more
;; sense, I think, because it reflects the idea that prerequisite data sources
;; are associated with the user of the Lisp image -- what data they are able
;; to get at from this terminal.
(defgeneric register-data-source (type &key)
  (:documentation
   "Initialise and register a source of prerequisite data in this Lisp image.
Registered data sources are available to all deployments executed from the
root Lisp, regardless of the consfig which defines the host to which
properties are to be applied.  (This could only cause problems if you have
different consfigs with prerequisite data which is identified by the same two
strings, in which case you will need to wrap your deployments with registering
and unregistering data sources.  Usually items of prerequisite data are
identified using things like hostnames, so this shouldn't be necessary.)

Implementations of this function return a pair of functions.

Signals a condition MISSING-DATA-SOURCE when unable to access the data source
(e.g. because can't decrypt it).  This condition is captured and ignored in
all new Lisp images started up by Consfigurator, since prerequisite data
sources are not expected to be available outside of the root Lisp."))

(define-simple-error missing-data-source)

(defvar *data-sources* nil "Known sources of prerequisite data.")

(defvar *data-source-registrations* nil
  "Successful attempts to register data sources, which need not be repeated.")

(defvar *no-data-sources* nil
  "If t, silently fail to register any data sources.")

(defvar *string-data* (make-hash-table :test #'equal)
  "Items of STRING-DATA obtained from data sources by this Lisp image.")

(defun try-register-data-source (&rest args)
  "Register sources of prerequisite data.
This function is typically called in consfigs.  Any relative pathnames in ARGS
will be resolved as paths under the home directory of the user Lisp is running
as, before being passed to implementations of REGISTER-DATA-SOURCE."
  (unless *no-data-sources*
    (let ((home (user-homedir-pathname)))
      (setq args
            (loop
              for arg in args
              if (pathnamep arg)
                collect (ensure-pathname arg :defaults home :ensure-absolute t)
              else collect arg)))
    (when-let ((pair (and (not (find args *data-source-registrations*
                                     :test #'equal))
                          (restart-case (apply #'register-data-source args)
                            (skip-data-source () nil)))))
      (push pair *data-sources*)
      (push args *data-source-registrations*))))

(defun skip-data-source (c)
  (declare (ignore c))
  (invoke-restart 'skip-data-source))

(defun reset-data-sources ()
  "Forget all data sources registered in this Lisp image and items of string
data obtained from data sources by this Lisp image.
This function is typically called at the REPL."
  (setq *string-data* (clrhash *string-data*)
        *data-sources* nil
        *data-source-registrations* nil))

(defun get-data-string (iden1 iden2)
  "Return the content of an item of prerequisite data as a string.

This function is called by property :APPLY and :UNAPPLY subroutines."
  (%get-data-string (funcall (%get-data iden1 iden2))))

(defun get-data-stream (iden1 iden2)
  "Return a stream which will produce the content of an item of prerequisite
data.  The elements of the stream are always octets.  If the item of
prerequisite data was provided by the prerequisite data source as a string, it
will be encoded in UTF-8.

This function is called by property :APPLY and :UNAPPLY subroutines."
  (%get-data-stream (funcall (%get-data iden1 iden2))))

(defmacro with-data-stream ((s iden1 iden2) &body body)
  `(with-open-stream (,s (get-data-stream ,iden1 ,iden2))
     ,@body))

(define-condition missing-data (error)
  ((iden1 :initarg :iden1 :reader missing-iden1)
   (iden2 :initarg :iden2 :reader missing-iden2))
  (:report (lambda (condition stream)
             (format stream "Could not provide prerequisite data ~S | ~S"
                     (missing-iden1 condition) (missing-iden2 condition)))))

(defun %get-data (iden1 iden2)
  (let* ((idenpair (cons iden1 iden2))
         (from-source (query-data-sources iden1 iden2))
         (from-source-version (and from-source (car from-source)))
         (in-memory (gethash idenpair *string-data*))
         (in-memory-version (and in-memory (data-version in-memory)))
         (local-cached
           (car (remove-if-not (lambda (c)
                                 (and (string= (first c) iden1)
                                      (string= (second c) iden2)))
                               (sort-prerequisite-data-cache
                                (get-local-cached-prerequisite-data)))))
         (local-cached-version (caddr local-cached)))
    (cond
      ((and in-memory
            (or (not from-source) (version>= in-memory-version
                                             from-source-version))
            (or (not local-cached) (version>= in-memory-version
                                              local-cached-version)))
       (informat 3 "~&Obtaining ~S | ~S from in-memory cache" iden1 iden2)
       (values (lambda () in-memory) in-memory-version))
      ((and from-source
            (or (not in-memory) (version>= from-source-version
                                           in-memory-version))
            (or (not local-cached) (version>= from-source-version
                                              local-cached-version)))
       (informat 3 "~&Obtaining ~S | ~S from a data source" iden1 iden2)
       (values
        (lambda ()
          (aprog1 (funcall (cdr from-source))
            (when (subtypep (type-of it) 'string-data)
              (setf (gethash idenpair *string-data*) it))))
        from-source-version))
      ((and local-cached
            (or (not from-source) (version>= local-cached-version
                                             from-source-version))
            (or (not in-memory) (version>= local-cached-version
                                           in-memory-version)))
       (informat 3 "~&Obtaining ~S | ~S from local cache" iden1 iden2)
       (values
        (lambda ()
          (let ((file (apply #'local-data-pathname local-cached)))
            (make-instance 'file-data
                           :iden1 iden1
                           :iden2 iden2
                           :version local-cached-version
                           :file file
                           :mime (try-get-file-mime-type file))))
        local-cached-version))
      (t
       (error 'missing-data :iden1 iden1 :iden2 iden2)))))

(defmethod %get-data-stream ((data string-data))
  (babel-streams:make-in-memory-input-stream
   (babel:string-to-octets (data-string data) :encoding :UTF-8)
   :element-type '(unsigned-byte 8)))

(defmethod %get-data-stream ((data file-data))
  (open (data-file data) :direction :input
                         :element-type '(unsigned-byte 8)))

(defmethod %get-data-string ((data string-data))
  (data-string data))

(defmethod %get-data-string ((data file-data))
  (read-file-string (data-file data)))

(defun query-data-sources (iden1 iden2)
  (flet ((make-thunk (v iden1 iden2)
           (lambda ()
             (funcall v iden1 iden2))))
    (car (sort (loop for (ver . get) in *data-sources*
                     for version = (funcall ver iden1 iden2)
                     when version
                       collect (cons version (make-thunk get iden1 iden2)))
               (lambda (x y)
                 (version> (car x) (car y)))))))

(defun data-source-providing-p (iden1 iden2)
  "Is there a data source which can provide the item of prerequisite data
identified by IDEN1 and IDEN2?

This function is for implementation of REGISTER-DATA-SOURCE to check for
clashes.  It should not be called by properties."
  (if (query-data-sources iden1 iden2) t nil))

(defun maybe-writefile-data (path iden1 iden2 &key (mode nil mode-supplied-p))
  "Wrapper around WRITEFILE which returns :NO-CHANGE and avoids touching PATH if
PATH's content is already the prerequisite data identified by IDEN1 and IDEN2
and PATH has mode MODE."
  (let ((data (funcall (%get-data iden1 iden2))))
    (etypecase data
      (string-data (apply #'maybe-writefile-string path (data-string data)
                          (and mode-supplied-p `(:mode ,mode))))
      (file-data
       (let ((stream (%get-data-stream data)))
         (if (and (remote-exists-p path)
                  (multiple-value-bind (existing-mode existing-size)
                      (remote-file-stats path)
                    (and (or (not mode-supplied-p) (= mode existing-mode))
                         (= (file-length stream) existing-size)
                         (= (data-cksum data) (cksum path)))))
             :no-change
             (apply #'writefile path stream
                    (and mode-supplied-p `(:mode ,mode)))))))))

(defgeneric connection-upload (connection data)
  (:documentation
   "Subroutine to upload an item of prerequisite data to the remote cache.
The default implementation will work for any connection which implements
CONNECTION-WRITEFILE and CONNECTION-RUN, but connection types which work by
calling CONTINUE-DEPLOY* or CONTINUE-DEPLOY*-PROGRAM will need their own
implementation."))

(defmethod connection-upload ((connection connection) (data data))
  (flet ((upload (from to)
           (with-open-file (stream from :element-type '(unsigned-byte 8))
             (writefile to stream))))
    (with-slots (iden1 iden2 data-version) data
      (informat 1 "~&Uploading (~@{~S~^ ~}) ... " iden1 iden2 data-version)
      (let* ((*connection* connection)
             (dest (remote-data-pathname iden1 iden2 data-version))
             (destdir (pathname-directory-pathname dest))
             (destfile (pathname-file dest)))
        (mrun "mkdir" "-p" destdir)
        (with-remote-current-directory (destdir)
          (etypecase data
            (string-data
             (writefile destfile (data-string data)))
            (file-data
             (let ((source (unix-namestring (data-file data))))
               (if (string-prefix-p "text/" (data-mime data))
                   (let ((destfile (strcat destfile ".gz")))
                     (with-temporary-file (:pathname tmp)
                       (run-program
                        (strcat "gzip -c " (escape-sh-token source))
                        :output tmp)
                       (upload tmp destfile)
                       (mrun "gunzip" destfile)))
                   (upload source destfile)))))))))
  (inform 1 "done." :fresh-line nil))

(defgeneric connection-clear-data-cache (connection iden1 iden2)
  (:documentation
   "Delete all versions of the data identified by IDEN1 and IDEN2 from the remote
cache of CONNECTION.  Called by UPLOAD-ALL-PREREQUISITE-DATA before uploading
new versions of data, to avoid them piling up."))

(defmethod connection-clear-data-cache ((connection connection) iden1 iden2)
  (let* ((*connection* connection)
         (dir (ensure-directory-pathname (remote-data-pathname iden1 iden2))))
    (delete-remote-trees dir)))

(defmethod connection-connattr
    ((connection connection) (k (eql 'cached-data)))
  (make-hash-table :test #'equal))

(defun upload-all-prerequisite-data (&optional (connection *connection*))
  "Upload all prerequisite data required by the current deployment to the remote
cache of the current connection hop, or to the remote cache of CONNECTION.

This is called by implementations of ESTABLISH-CONNECTION which call
CONTINUE-DEPLOY* or CONTINUE-DEPLOY*-PROGRAM."
  (flet ((record-cached-data (iden1 iden2 version)
           (let ((*connection* connection))
             (setf (gethash (cons iden1 iden2) (get-connattr 'cached-data))
                   (remote-data-pathname iden1 iden2 version)))))
    (loop with *data-sources* = (cons (register-data-source :asdf)
                                      *data-sources*)
          with remote-cached
            = (sort-prerequisite-data-cache
               (get-remote-cached-prerequisite-data connection))

          for (iden1 . iden2) in (get-hostattrs :data)
          for highest-remote-version
            = (caddar (remove-if-not (lambda (c)
                                       (and (string= (first c) iden1)
                                            (string= (second c) iden2)))
                                     remote-cached))
          for (thunk highest-local-version)
            = (handler-case (multiple-value-list (%get-data iden1 iden2))
                (missing-data () nil))

          if (and highest-local-version
                  (or (not highest-remote-version)
                      (version> highest-local-version highest-remote-version)))
            do (let ((data (funcall thunk)))
                 (connection-clear-data-cache connection iden1 iden2)
                 (connection-upload connection data)
                 (record-cached-data iden1 iden2 (data-version data)))
          else if highest-remote-version
                 do (informat 3 "~&Not uploading ~S | ~S ver ~S as remote has ~S"
                              iden1 iden2
                              highest-local-version highest-remote-version)
                    (record-cached-data iden1 iden2 highest-remote-version)
          else do (error 'missing-data :iden1 iden1 :iden2 iden2))))

(defun try-get-file-mime-type (file)
  (handler-case (stripln (run-program
                          (escape-sh-command (list "file" "-E"
                                                   "--mime-type" "--brief"
                                                   (unix-namestring file)))
                          :output :string))
    (subprocess-error () nil)))

(defun sort-prerequisite-data-cache (cache)
  (sort cache (lambda (x y) (version> (third x) (third y)))))

(defun data-pathname (root &rest segments)
  (destructuring-bind (last . rest)
      (nreverse (mapcar #'string->filename segments))
    (merge-pathnames last (reduce #'merge-pathnames
                                  (mapcar (lambda (s) (strcat s "/")) rest)
                                  :from-end t :initial-value root))))

(defun local-data-pathname (&optional iden1 iden2 version)
  "Get a pathname where an item of prerequisite data may be cached, ensuring
that parent directories exist.
This is exported for use by prerequisite data sources which work by generating
new files and need somewhere to store them.  It should not be used by
properties, or data sources which return objects referencing existing files.

Note that since prerequisite data sources are queried only in the root Lisp,
but items of prerequisite data are never uploaded to the root Lisp, there is
no risk of clashes between fresly generated files and cached copies of files."
  (let ((pn (apply #'data-pathname (get-local-data-cache-dir)
		   (delete nil (list iden1 iden2 version)))))
    (ensure-directories-exist
     (if version pn (ensure-directory-pathname pn)))))

(defun remote-data-pathname (&rest args)
  (apply #'data-pathname
         (merge-pathnames "data/" (get-connattr :consfigurator-cache)) args))


;;;; Remote caches

(defgeneric get-remote-cached-prerequisite-data (connection)
  (:documentation
   "Return a list of items of prerequisite data in the cache on the remote side
of CONNECTION, where each entry is of the form

    '(iden1 iden2 version)."))

(defmethod get-remote-cached-prerequisite-data ((connection connection))
  (let ((*connection* connection))
    (mapcar (lambda (line)
              (mapcar #'filename->string (split-string line :separator "/")))
            (multiple-value-bind (out exit)
                (mrun :may-fail "find" (merge-pathnames
                                        "data/"
                                        (get-connattr :consfigurator-cache))
                      "-type" "f" "-printf" "%P\\n")
              (and (zerop exit) (lines out))))))


;;;; Local caches

(defun get-local-cached-prerequisite-data
    (&optional (where (get-local-data-cache-dir)))
  "Scan a local cache of prerequisite data at WHERE, and return a list of
items of prerequisite data where each entry is of the form

    '(iden1 iden2 version).

This is exported for use by implementations of CONNECTION-UPLOAD, which should
always supply a value for WHERE."
  (loop for dir in (subdirectories where)
        nconc (loop for subdir in (subdirectories dir)
                    nconc (loop for file in (directory-files subdir)
                                collect
                                (mapcar #'filename->string
                                        (list (lastcar
                                               (pathname-directory dir))
                                              (lastcar
                                               (pathname-directory subdir))
                                              (pathname-file file)))))))

(defun get-highest-local-cached-prerequisite-data (iden1 iden2)
  "Get the highest version of prerequisite data identified by IDEN1 and IDEN2
available in the local cache.

This is exported for use by prerequisite data sources which work by generating
new files and need somewhere to store them.  It should not be used by
properties, or data sources which return objects referencing existing files."
  (when-let ((triple (car (remove-if-not
			   (lambda (c)
			     (and (string= (car c) iden1)
				  (string= (cadr c) iden2)))
			   (sort-prerequisite-data-cache
			    (get-local-cached-prerequisite-data))))))
    (make-instance 'file-data :file (apply #'local-data-pathname triple)
			      :iden1 (car triple)
			      :iden2 (cadr triple)
			      :version (caddr triple))))

(defun get-local-data-cache-dir ()
  (merge-pathnames
   "data/"
   ;; A combinator like WITH-HOMEDIR might have temporarily set the HOME
   ;; and/or XDG_CACHE_HOME environment variables, so use a cached value if we
   ;; can find one.
   (or (loop for conn = *connection* then (connection-parent conn)
             while conn
             when (subtypep (type-of conn) 'lisp-connection)
               return (connection-connattr conn :consfigurator-cache))
       (connection-connattr
        (establish-connection :local nil) :consfigurator-cache))))


;;;; Passphrases

(defclass passphrase ()
  ((passphrase :initarg :passphrase :reader passphrase)))

(defun make-passphrase (passphrase)
  "Make an object which is unprintable by default to contain a passphrase."
  (make-instance 'passphrase :passphrase passphrase))

(defun get-data-protected-string (iden1 iden2)
  "Like GET-DATA-STRING, but wrap the content in an object which is unprintable
by default.  Intended for code which fetches passwords and wants to lessen the
chance of those passwords showing up in the clear in the Lisp debugger."
  (make-passphrase (get-data-string iden1 iden2)))

(defvar *allow-printing-passphrases* nil)

(defmethod print-object ((passphrase passphrase) stream)
  (if *allow-printing-passphrases*
      (format stream "#.~S"
              `(make-instance 'passphrase
                              :passphrase ,(passphrase passphrase)))
      (print-unreadable-object (passphrase stream)
        (format stream "PASSPHRASE")))
  passphrase)
