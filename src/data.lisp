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
          (let ((from-source-data (funcall (cdr from-source))))
            (when (subtypep (type-of from-source-data) 'string-data)
              (setf (gethash idenpair *string-data*) from-source-data))
            from-source-data))
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
                      (remote-file-mode-and-size path)
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

(defun upload-all-prerequisite-data
    (&key (upload-string-data t) (connection *connection*))
  "Upload all prerequisite data required by the current deployment to the remote
cache of the current connection hop, or to the remote cache of CONNECTION.

If UPLOAD-STRING-DATA is false, don't upload items of string data, but
retrieve them from data sources and keep in memory.  This is for connection
types which will do something like fork after calling this function.

This is called by implementations of ESTABLISH-CONNECTION which call
CONTINUE-DEPLOY* or CONTINUE-DEPLOY*-PROGRAM."
  ;; Retrieving & keeping in memory refers to how %GET-DATA stores items of
  ;; string data in *STRING-DATA*.
  (flet ((record-cached-data (iden1 iden2 version)
           (let ((*connection* connection))
             (setf (gethash (cons iden1 iden2) (get-connattr 'cached-data))
                   (remote-data-pathname iden1 iden2 version)))))
    (loop with *data-sources* = (cons (register-data-source :asdf)
                                      *data-sources*)

          for (iden1 . iden2) in (get-hostattrs :data)
          for highest-remote-version
            = (caddar (remove-if-not (lambda (c)
                                       (and (string= (first c) iden1)
                                            (string= (second c) iden2)))
                                     (sort-prerequisite-data-cache
                                      (get-remote-cached-prerequisite-data
                                       connection))))
          for (thunk highest-local-version)
            = (restart-case (multiple-value-list (%get-data iden1 iden2))
                (missing-data () nil))

          if (and highest-local-version
                  (or (not highest-remote-version)
                      (version> highest-local-version highest-remote-version)))
            do (let ((data (funcall thunk)))
                 (when (or upload-string-data
                           (not (subtypep (type-of data) 'string-data)))
                   (connection-clear-data-cache connection iden1 iden2)
                   (connection-upload connection data)
                   (record-cached-data iden1 iden2 (data-version data))))
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
		   (delete-if #'null (list iden1 iden2 version)))))
    (ensure-directories-exist
     (if version pn (ensure-directory-pathname pn)))))

(defun remote-data-pathname (&rest args)
  (apply #'data-pathname (get-remote-data-cache-dir) args))


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
                (mrun :may-fail "find" (get-remote-data-cache-dir)
                      "-type" "f" "-printf" "%P\\n")
              (and (zerop exit) (lines out))))))

(defun get-remote-data-cache-dir ()
  (ensure-directory-pathname
   (car
    (lines
     (mrun "echo ${XDG_CACHE_HOME:-$HOME/.cache}/consfigurator/data/")))))


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
  (ensure-directory-pathname
   (strcat (or (getenv "XDG_CACHE_HOME") (strcat (getenv "HOME") "/.cache"))
           "/consfigurator/data")))


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


;;;; Programs for remote Lisp images

(defclass asdf-requirements ()
  ((asdf-requirements :type list :initform nil))
  (:documentation
   "A list of requirements as returned by certain calls to
ASDF:REQUIRED-COMPONENTS.
Elements are instances of ASDF:SYSTEM and/or ASDF:REQUIRE-SYSTEM."))

(defun asdf-requirements-for-host-and-features (remote-lisp-features)
  "Make an instance of ASDF-REQUIREMENTS for starting up a remote Lisp image in
which *FEATURES* has the value of REMOTE-LISP-FEATURES, based on the Lisp
systems required by the host currently being deployed.

Called by connection types which start up remote Lisp images."
  (let ((*features* remote-lisp-features)
        (requirements (make-instance 'asdf-requirements)))
    (with-slots (asdf-requirements) requirements
      (dolist (system (propspec-systems (host-propspec *host*)))
        (dolist (requirement
                 ;; This call to ASDF:REQUIRED-COMPONENTS is based on one in
                 ;; the definition of the ASDF:COMPONENT-DEPENDS-ON generic
                 ;; for ((o gather-operation) (s system)).  We use
                 ;; ASDF:COMPILE-OP as the :KEEP-OPERATION because
                 ;; ASDF::BASIC-COMPILE-OP is not exported, so this won't work
                 ;; for certain exotic systems.  See the comment in ASDF source.
                 ;;
                 ;; TODO Can we detect when this won't work and fail, possibly
                 ;; falling back to ASDF:MONOLITHIC-CONCATENATE-SOURCE-OP?
                 (asdf:required-components
                  (asdf:find-system system)
                  :other-systems t :component-type 'asdf:system
                  :keep-component 'asdf:system :goal-operation 'asdf:load-op
                  :keep-operation 'asdf:compile-op))
          ;; Handle UIOP specially because it comes with ASDF.
          (unless (string= "uiop" (asdf:component-name requirement))
            ;; What we really want instead of PUSHNEW here is a proper
            ;; topological sort.
            (pushnew requirement asdf-requirements))))
      (nreversef asdf-requirements))
    requirements))

(defgeneric request-asdf-requirements (asdf-requirements)
  (:documentation
   "Request that all Lisp systems required to fulfill ASDF-REQUIREMENTS be
uploaded to the remote cache of the currently established connection.

Called by connection types which start up remote Lisp images.")
  (:method ((asdf-requirements asdf-requirements))
    (loop for requirement in (slot-value asdf-requirements 'asdf-requirements)
          for type = (type-of requirement)
          when (and (subtypep type 'asdf:system)
                    (not (subtypep type 'asdf:require-system)))
            do (require-data "--lisp-system"
                             (asdf:component-name requirement)))))

(defgeneric asdf-requirements-load-form (asdf-requirements)
  (:documentation
   "Return form to (compile and) load each of the Lisp systems specified in
ASDF-REQUIREMENTS, after having uploaded those Lisp systems using
UPLOAD-ALL-PREREQUISITE-DATA.")
  (:method ((asdf-requirements asdf-requirements))
    ;; As soon as we recompile something, we have to recompile everything else
    ;; following it in the list, because macro definitions may have changed.
    `(let (recompile)
       ,@(loop with table = (get-connattr 'cached-data)
               for requirement in (slot-value asdf-requirements 'asdf-requirements)
               for name = (asdf:component-name requirement)
               collect
               (etypecase requirement
                 (asdf:require-system `(require ,name))
                 (asdf:system
                  (let ((source (gethash (cons "--lisp-system" name) table)))
                    (unless source
                      (error "Somehow Lisp system ~A was not uploaded." name))
                    ;; TODO Using COMPILE-FILE-PATHNAME* like this has the
                    ;; advantage that, for example, SBCL will save the FASL
                    ;; somewhere from which only the same version of SBCL will
                    ;; try to load FASLs.  However, FASLs corresponding to old
                    ;; versions of Lisp systems are not cleaned up, and are
                    ;; not tiny files.
                    `(let ((fasl (compile-file-pathname* ,source)))
                       (if (and (file-exists-p fasl) (not recompile))
                           (load fasl)
                           ;; The concatenated source of at least Alexandria
                           ;; won't compile unless it's loaded first.  This
                           ;; means we compile every library that's changed
                           ;; since the last deploy twice, which is not ideal.
                           ;; One possible improvement would be to maintain a
                           ;; list of systems known not to have this problem,
                           ;; such as Consfigurator, and switch the order of
                           ;; the LOAD and COMPILE-FILE* here for those.
                           (progn (load ,source)
                                  (compile-file* ,source)
                                  (setq recompile t)))))))))))

(defgeneric continue-deploy*-program (remaining-connections asdf-requirements)
  (:documentation
   "Return a program to complete the work of an enclosing call to DEPLOY*.

Implementations of ESTABLISH-CONNECTION which start up remote Lisp images call
this function, instead of CONTINUE-DEPLOY*, and use the result to instruct the
newly started image.

Will query the remote cache for paths to Lisp systems, so a connection to the
host which will run the Lisp image must already be established.

The program returned is a single string consisting of a number of sexps
separated by newlines.  Each sexp must be evaluated by the remote Lisp image
before the following sexp is offered to its reader.  Usually this can be
achieved by sending the return value of this function into a REPL's stdin.")
  (:method (remaining-connections (asdf-requirements asdf-requirements))
    (unless (eq (type-of *host*) 'preprocessed-host)
      (error "Attempt to send unpreprocessed host to remote Lisp.

Preprocessing must occur in the root Lisp."))
    (flet ((wrap (form)
             ;; We used to bind a handler here to invoke SKIP-DATA-SOURCES
             ;; upon MISSING-DATA-SOURCE, which means that remote Lisp images
             ;; were allowed to try querying data sources.  Now we just bind
             ;; *NO-DATA-SOURCES* to t here.  While some data sources make
             ;; sense in remote Lisp images, others might make arbitrary
             ;; network connections or read out of other users' homedirs
             ;; (e.g. if you are using (:SUDO :SBCL), the remote Lisp might
             ;; try to read your ~/.gnupg, or on another host, someone else's
             ;; ~/.gnupg who has the same username as you), which are usually
             ;; undesirable.  So at least until some cool use case comes
             ;; along, just require all data source queries to occur in the
             ;; root Lisp.
             `(let ((*no-data-sources* t)
                    (*consfigurator-debug-level* ,*consfigurator-debug-level*))
                ,form)))
      (let* ((intern-forms
               (loop for (export . name)
                       in '((nil . "*NO-DATA-SOURCES*")
                            (t . "*CONSFIGURATOR-DEBUG-LEVEL*"))
                     for intern-form
                       = `(intern ,name (find-package "CONSFIGURATOR"))
                     if export collect
                       `(export ,intern-form (find-package "CONSFIGURATOR"))
                     else collect intern-form))
             (proclamations `((proclaim '(special *no-data-sources*))
                              (proclaim '(special *consfigurator-debug-level*))))
             (forms
               `((make-package "CONSFIGURATOR")
                 ,@intern-forms
                 ,@proclamations
                 ;; (define-condition missing-data-source (error) ())
                 (require "asdf")
                 ;; Hide the compile and/or load output unless there are
                 ;; failures or the debug level is at least 3, as it's verbose
                 ;; and not usually of interest.
                 ,(wrap
                   `(let ((string
                            (make-array '(0) :element-type 'character
                                             :fill-pointer 0 :adjustable t)))
                      (handler-case
                          (with-output-to-string (stream string)
                            (let ((*error-output* stream)
                                  (*standard-output* stream))
                              ,(asdf-requirements-load-form
                                asdf-requirements)))
                        (serious-condition (c)
                          (format
                           *error-output*
                           "~&Failed to compile and/or load:~%~A~&~%Compile and/or load output:~%~%~A"
                           c string)
                          (uiop:quit 2)))
                      (when (>= *consfigurator-debug-level* 3)
                        (format t "~&~A" string))))
                 ,(wrap `(%consfigure ',remaining-connections ,*host*)))))
        (handler-case
            (with-standard-io-syntax
              (let ((*allow-printing-passphrases* t))
                ;; need line breaks in between so that packages exist before we
                ;; try to have remote Lisp read sexps containing symbols from
                ;; those packages
                (values
                 (format nil "~{~A~^~%~}" (mapcar #'prin1-to-string forms))
                 forms)))
          (print-not-readable (c)
            (error "The Lisp printer could not serialise ~A for
transmission to the remote Lisp.

This is probably because your property application specification and/or static
informational attributes contain values which the Lisp printer does not know
how to print.  If ~:*~A is something like a function object then you need to
rework your deployment so that it does not end up in the propspec or
hostattrs; see \"Pitfalls\" in the Consfigurator user manual.

If ~:*~A is a simple object then you may be able to resolve this by defining
a PRINT-OBJECT method for your class, possibly using
CONSFIGURATOR:DEFINE-PRINT-OBJECT-FOR-STRUCTLIKE."
                   (print-not-readable-object c))))))))
