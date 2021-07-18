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

;;;; Remote Lisp images

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
    `(let* (recompile
            (file (merge-pathnames "consfigurator/fasls"
                                   (ensure-directory-pathname
                                    (or (getenv "XDG_CACHE_HOME")
                                        (strcat (getenv "HOME") "/.cache")))))
            (record (with-open-file (stream file :if-does-not-exist nil)
                      (and stream (safe-read-from-string
                                   (slurp-stream-string stream))))))
       (unwind-protect
            (progn
              ,@(loop
                  with table = (get-connattr 'cached-data)
                  for requirement
                    in (slot-value asdf-requirements 'asdf-requirements)
                  for name = (asdf:component-name requirement)
                  collect
                  (etypecase requirement
                    (asdf:require-system `(require ,name))
                    (asdf:system
                     (let ((source
                             (gethash (cons "--lisp-system" name) table)))
                       (unless source
                         (error "Somehow Lisp system ~A was not uploaded."
                                name))
                       ;; Using COMPILE-FILE-PATHNAME* like this has the
                       ;; advantage that, for example, SBCL will save the FASL
                       ;; somewhere from which only the same version of SBCL
                       ;; will try to load FASLs.
                       `(let ((fasl (compile-file-pathname* ,source)))
                          (if (and (file-exists-p fasl) (not recompile))
                              (load fasl)
                              ;; The concatenated source of at least
                              ;; Alexandria won't compile unless it's loaded
                              ;; first.  This means we compile every library
                              ;; that's changed since the last deploy twice,
                              ;; which is not ideal.  One possible improvement
                              ;; would be to maintain a list of systems known
                              ;; not to have this problem, such as
                              ;; Consfigurator, and switch the order of the
                              ;; LOAD and COMPILE-FILE* here for those.
                              (let ((pair (assoc ,source record)))
                                (load ,source)
                                (or (compile-file* ,source)
                                    (error "Failed to compile ~S" ,source))
                                (if pair
                                    (rplacd pair fasl)
                                    (setq record (acons ,source fasl record)))
                                (setq recompile t)))))))))
         (with-open-file (stream file :direction :output :if-exists :supersede)
           (with-standard-io-syntax
             (prin1 record stream)))))))

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
                 ;; Delete old FASLs.  With SBCL they are megabytes in size.
                 (with-lisp-data-file
                     (record (merge-pathnames
                              "consfigurator/fasls"
                              (ensure-directory-pathname
                               (or (getenv "XDG_CACHE_HOME")
                                   (strcat (getenv "HOME") "/.cache")))))
                   (loop for cell in record
                         if (file-exists-p (car cell))
                           collect cell into accum
                         else do (ignore-errors (delete-file (cdr cell)))
                         finally (setq record accum)))
                 ;; Continue the deployment.
                 ,(wrap
                   `(with-backtrace-and-exit-code
                      (%consfigure ',remaining-connections ,*host*))))))
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
