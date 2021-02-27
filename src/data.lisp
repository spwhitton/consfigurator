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
    :reader data-mime
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
  ((data-file
    :initarg :file
    :reader data-file))
  (:documentation
   "An item of prerequisite data accessible via the filesystem."))

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

(define-condition missing-data-source (error)
  ((text :initarg :text :reader missing-data-source-text))
  (:report (lambda (condition stream)
	     (format stream "Missing data source: ~A"
		     (missing-data-source-text condition)))))

(defvar *data-sources* nil "Known sources of prerequisite data.")

(defvar *data-source-registrations* nil
  "Successful attempts to register data sources, which need not be repeated.")

(defun try-register-data-source (&rest args)
  "Register sources of prerequisite data.
This function is typically called in consfigs."
  (when-let ((pair (and (not (find args *data-source-registrations*
				   :test #'equal))
			(restart-case (apply #'register-data-source args)
			  (skip-data-source () nil)))))
    (push pair *data-sources*)
    (push args *data-source-registrations*)))

(defun skip-data-source (c)
  (declare (ignore c))
  (invoke-restart 'skip-data-source))

(defun reset-data-sources ()
  "Forget all data sources registered in this Lisp image.
This function is typically called at the REPL."
  (setq *data-sources* nil
	*data-source-registrations* nil))

(defun get-data-string (iden1 iden2)
  "Return the content of an item of prerequisite data as a string.

This function is called by property :APPLY and :UNAPPLY subroutines."
  (%get-data-string (%get-data iden1 iden2)))

(defun get-data-stream (iden1 iden2)
  "Return a stream which will produce the content of an item of prerequisite
data.  The elements of the stream are always octets.  If the item of
prerequisite data was provided by the prerequisite data source as a string, it
will be encoded in UTF-8.

This function is called by property :APPLY and :UNAPPLY subroutines."
  (%get-data-stream (%get-data iden1 iden2)))

(defmacro with-data-stream ((s iden1 iden2) &body body)
  `(with-open-stream (,s (get-data-stream ,iden1 ,iden2))
     ,@body))

(defun %get-data (iden1 iden2)
  (if-let ((source-thunk (cdr (query-data-sources iden1 iden2))))
    (funcall source-thunk)
    ;; else, look in local cache -- note that this won't exist in the root
    ;; Lisp, but only if we're a Lisp started up by a connection
    (if-let ((local-cached
	      (car (remove-if-not (lambda (c)
				    (and (string= (first c) iden1)
					 (string= (second c) iden2)))
				  (sort-prerequisite-data-cache
				   (get-local-cached-prerequisite-data))))))
      (let ((file (apply #'local-data-pathname local-cached)))
	(make-instance 'file-data
		       :iden1 iden1
		       :iden2 iden2
		       :file file
		       :mime (try-get-file-mime-type file)))
      (error "Could not provide prerequisite data ~S | ~S" iden1 iden2))))

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

;; called by implementations of ESTABLISH-CONNECTION which start up remote
;; Lisp images
(defun upload-all-prerequisite-data (&optional (host *host*))
  (macrolet ((highest-version-in-cache (cache)
	       `(third (car (remove-if-not (lambda (c)
					     (and (string= (first c) iden1)
						  (string= (second c) iden2)))
					   ,cache)))))
    (loop with *data-sources* = (cons (register-data-source :asdf)
				      *data-sources*)

	  with sorted-local-cache  = (sort-prerequisite-data-cache
				      (get-local-cached-prerequisite-data))
	  with sorted-remote-cache = (sort-prerequisite-data-cache
				      (get-remote-cached-prerequisite-data))
	  for (iden1 . iden2) in (getf (slot-value host 'hostattrs) :data)

	  for highest-local-cached-version  = (highest-version-in-cache
					       sorted-local-cache)
	  for highest-remote-cached-version = (highest-version-in-cache
					       sorted-remote-cache)
	  for (highest-source-version . highest-source)
	    = (query-data-sources iden1 iden2)

	  if (and highest-source-version
		  (or (not highest-remote-cached-version)
		      (version< highest-remote-cached-version
				highest-source-version)))
	    do (connection-clear-data-cache iden1 iden2)
	       (connection-upload-data (funcall highest-source))
	  else if (and highest-local-cached-version
		       (or (not highest-remote-cached-version)
			   (version< highest-remote-cached-version
				     highest-local-cached-version)))
		 do (let ((file (local-data-pathname
				 iden1
				 iden2
				 highest-local-cached-version)))
		      (connection-clear-data-cache iden1 iden2)
		      (connection-upload-data
		       (make-instance 'file-data
				      :iden1 iden1
				      :iden2 iden2
				      :version highest-local-cached-version
				      :file file
				      :mime (try-get-file-mime-type file))))
	  else unless highest-remote-cached-version
		 do (error "Could not provide prerequisite data ~S | ~S"
			   iden1 iden2))))

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

(defun local-data-pathname (&rest args)
  (apply #'data-pathname (get-local-data-cache-dir) args))

(defun remote-data-pathname (&rest args)
  (apply #'data-pathname (get-remote-data-cache-dir) args))

(defun connection-try-upload (from to)
  "Wrapper around CONNECTION-UPLOAD to ensure it gets used only when
appropriate.  Falls back to CONNECTION-WRITEFILE."
  (if (and (subtypep (type-of (slot-value *connection* 'parent))
		     'consfigurator.connection.local:local-connection)
	   (find-method #'connection-upload
			'()
			(mapcar #'class-of (list *connection* t t))
			nil))
      (connection-upload *connection* from to)
      (with-open-file (s from :element-type '(unsigned-byte 8))
	(connection-writefile *connection* to s #o077))))

(defmethod connection-upload-data :around ((data data))
  (when (subtypep (class-of *connection*)
		  'consfigurator.connection.local:local-connection)
    (error "Attempt to upload data to the root Lisp; this is not allowed"))
  (with-slots (iden1 iden2 data-version) data
    (let ((*dest* (remote-data-pathname iden1 iden2 data-version)))
      (declare (special *dest*))
      (mrun "mkdir" "-p" (pathname-directory-pathname *dest*))
      (format t "Uploading (~@{~S~^ ~}) ... " iden1 iden2 data-version)
      (call-next-method)
      ;; TODO eliminate *{THIS,LAST}-HOP-INFO* and just store this in a field
      ;; of *CONNECTION*
      (push (list iden1 iden2 *dest*) (getf *this-hop-info* :cached-data))
      (format t "done.~%"))))

(defmethod connection-upload-data ((data file-data))
  (declare (special *dest*))
  (let ((source (unix-namestring (data-file data))))
    (if (string-prefix-p "text/" (data-mime data))
	(let ((dest (strcat (unix-namestring *dest*) ".gz")))
	  (with-temporary-file (:pathname tmp)
	    (run-program (strcat "gzip --rsyncable -c "
				 (escape-sh-token source))
			 :output tmp)
	    (connection-try-upload tmp (unix-namestring dest))
	    (mrun "gunzip" "--keep" dest)))
	(connection-try-upload source *dest*))))

(defmethod connection-upload-data ((data string-data))
  (declare (special *dest*))
  (connection-writefile *connection* *dest* (data-string data) #o077))

(defun connection-clear-data-cache (iden1 iden2)
  (let ((dir (ensure-directory-pathname (remote-data-pathname iden1 iden2))))
    (mrun (strcat "rm -f "
		  (unix-namestring (pathname-directory-pathname dir))
		  "/*"))))

(defun get-local-data-cache-dir ()
  (ensure-directory-pathname
   (strcat (or (getenv "XDG_CACHE_HOME") (strcat (getenv "HOME") "/.cache"))
	   "/consfigurator/data")))

(defun get-local-cached-prerequisite-data ()
  "Return a list of items of prerequisite data in the cache local to this Lisp
process, where each entry is of the form

    '(iden1 iden2 version)."
  (loop for dir in (subdirectories (get-local-data-cache-dir))
	nconc (loop for subdir in (subdirectories dir)
		    nconc (loop for file in (directory-files subdir)
				collect
				(mapcar #'filename->string
					(list (lastcar
					       (pathname-directory dir))
					      (lastcar
					       (pathname-directory subdir))
					      (pathname-name file)))))))

(defun get-remote-data-cache-dir ()
  (ensure-directory-pathname
   (car
    (lines
     (mrun "echo ${XDG_CACHE_HOME:-$HOME/.cache}/consfigurator/data/")))))

(defun get-remote-cached-prerequisite-data ()
  "Return a list of items of prerequisite data in the cache on the remote side
of the current connection, where each entry is of the form

    '(iden1 iden2 version)."
  (mapcar (lambda (line)
	    (mapcar #'filename->string (split-string line :separator "/")))
	  (multiple-value-bind (out exit)
	      (mrun :may-fail "find" (get-remote-data-cache-dir)
		    "-type" "f" "-printf" "%P\\n")
	    (and (= 0 exit) (lines out)))))

;; TODO on remote side, catch read errors and signal our own which says
;; something more specific -- "This has probably been caused by an attempt to
;; use a property application specification or set of static informational
;; attributes which cannot be serialised by the Lisp printer"
(defun deployment-handover-program (remaining)
  "Return a program which instructs a remote Lisp image to continue DEPLOY*.

Will query the remote cache for paths to Lisp systems, so a connection to the
host which will run the Lisp image must already be established.

Called by connections which start up remote Lisp images."
  (flet ((wrap (forms)
	   `(handler-bind
		(;; we can skip missing data sources because these are not
		 ;; expected to be available outside of the root Lisp
		 (missing-data-source
		   (lambda (c)
		     (declare (ignore c))
		     (invoke-restart 'skip-data-source)))
		 ;; we can skip missing components when our particular restart
		 ;; is available because we've already uploaded everything
		 ;; that was declared to be required
		 (asdf/find-component:missing-component
		   (lambda (c)
		     (declare (ignore c))
		     (let ((restart (find-restart 'continue-without-system)))
		       (when restart (invoke-restart restart))))))
	      ,@forms)))
    (let ((intern-forms
	    (loop for name in '("MISSING-DATA-SOURCE"
				"SKIP-DATA-SOURCE"
				"CONTINUE-WITHOUT-SYSTEM")
		  collect
		  `(export (intern ,name (find-package "CONSFIGURATOR"))
			   (find-package "CONSFIGURATOR"))))
	  (load-forms
	    (loop for system
		    in (slot-value (slot-value *host* 'propspec) 'systems)
		  collect `(load
			    ,(caddar
			      (remove-if-not
			       (lambda (d)
				 (string= (car d) "--lisp-system")
				 (string= (cadr d) (normalise-system system)))
			       (getf *this-hop-info* :cached-data))))))
	  (*package* (find-package "COMMON-LISP-USER")))
      ;; need line breaks in between so that packages exist before we try to
      ;; have remote Lisp read sexps containing symbols from those packages
      (format nil "~{~A~^~%~}"
	      (mapcar
	       #'prin1-to-string
	       `((make-package "CONSFIGURATOR")
		 ,@intern-forms
		 (define-condition missing-data-source (error) ())
		 (require "asdf")
		 (let ((*standard-output* *error-output*))
		   ,(wrap load-forms))
		 ,(wrap `((deploy* ,(or remaining :local) ,*host*)))))))))

(defun request-lisp-systems ()
  "Request that all Lisp systems required by the host currently being deployed
are uploaded to the remote cache of the currently established connection.

Called by connections which start up remote Lisp images."
  (dolist (system (slot-value (slot-value *host* 'propspec) 'systems))
    (push-hostattrs :data (cons "--lisp-system" (normalise-system system)))))
