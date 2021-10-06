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

;;;; Deployments

(defparameter *at-end-functions* nil)

(defun at-end (function)
  "Request that FUNCTION be called at the end of the current (sub)deployment.
Called by property :APPLY and :UNAPPLY subroutines.  FUNCTION will be passed a
single argument representing whether or not the deployment made a change.

Properties which call this are responsible for ensuring that the I/O performed
by FUNCTION is compatible with the connection type.  This amounts to the
following requirement: if FUNCTION performs I/O beyond what :POSIX property
:APPLY subroutines are permitted to perform, the property calling AT-END to
register FUNCTION must be declared to be a :LISP property."
  (push (ensure-function function) *at-end-functions*))

(defun %consfigure (connections host &key (collect-at-end t))
  "Consfigurator's primary loop, recursively binding *CONNECTION* and *HOST*.

Assumes arguments to connections in CONNECTIONS have been both normalised and
preprocessed."
  (labels
      ((apply-*host*-propspec ()
         (let ((propapp (eval-propspec (host-propspec *host*))))
           (assert-connection-supports (propapptype propapp))
           (if collect-at-end
               (let (*at-end-functions*)
                 (let ((result (propappapply propapp)))
                   (dolist (function *at-end-functions* result)
                     (funcall function result))))
               (propappapply propapp))))
       (connect (connections)
         (destructuring-bind ((type . args) . remaining) connections
           ;; implementations of ESTABLISH-CONNECTION which call
           ;; CONTINUE-DEPLOY* or CONTINUE-DEPLOY*-PROGRAM return nil to us,
           ;; and possibly :NO-CHANGE as a second value
           (multiple-value-bind (*connection* return)
               (apply #'establish-connection type remaining args)
             (if *connection*
                 (unwind-protect
                     (if remaining (connect remaining) (apply-*host*-propspec))
                   (connection-teardown *connection*))
                 return)))))
    (let ((*host* (preprocess-host host)))
      (cond
        ((and connections (or *connection* (eq :local (caar connections))))
         (connect connections))
        (connections
         (connect (cons '(:local) connections)))
        (*connection*
         (apply-*host*-propspec))
        (t
         (connect '((:local))))))))

(defun consfigure (propspec-expression &key collect-at-end)
  "Immediately preprocess and apply PROPSPEC-EXPRESSION in the context of the
current target host and connection.  This function is provided for use by
specialised property combinators.  It should not be used in property
definitions nor in consfigs.

The :HOSTATTRS subroutines of properties applied by PROPSPEC-EXPRESSION will
be executed, but any new hostattrs they push will be discarded.  Thus either
PROPSPEC-EXPRESSION should not apply any properties whose :HOSTATTRS
subroutines push new hostattrs, or the caller should seperately arrange for
those subroutines to be executed in a context in which newly pushed hostattrs
will not be discarded."
  (%consfigure
   nil (make-host
        :hostattrs (hostattrs *host*)
        :propspec (with-*host*-*consfig*
                    (make-propspec :propspec propspec-expression)))
   :collect-at-end collect-at-end))

(defun deploy* (connections host &optional additional-properties)
  "Execute the deployment which is defined by the pair (CONNECTIONS . HOST),
except possibly with the property application specification
ADDITIONAL-PROPERTIES also applied to HOST.

This is the entry point to Consfigurator's primary loop.  Typically users use
DEPLOY, DEPLOY-THESE, and the function definitions established by DEFDEPLOY,
DEFDEPLOY-THESE, etc., rather than calling this function directly.  However,
code which programmatically constructs deployments will need to call this."
  (with-deployment-report
      (%consfigure (preprocess-connections connections)
                   (union-propspec-into-host host additional-properties))))

(defun deploy-these* (connections host properties)
  "Like DEPLOY*, but replace the properties of HOST with PROPERTIES.

HOST has all its usual static informational attributes, as set by its usual
properties, plus any set by PROPERTIES.  Static informational attributes set
by PROPERTIES can override the host's usual static informational attributes,
in the same way that later entries in the list of properties specified in
DEFHOST forms can override earlier entries (see DEFHOST's docstring)."
  (with-deployment-report
      (%consfigure (preprocess-connections connections)
                   (replace-propspec-into-host host properties))))

(defun continue-deploy* (connection remaining-connections)
  "Complete the work of an enclosing call to DEPLOY* or DEPLOY-THESE*.

Used by implementations of ESTABLISH-CONNECTION which need to do something
like fork(2) and then return to Consfigurator's primary loop in the child."
  (let ((*connection* connection))
    (%consfigure remaining-connections *host*)))

;; in the following two macros, bind *HOST* so that evaluation of the
;; unevaluated propspec can retrieve existing hostattrs; shallow copy just in
;; case the evaluation of the arguments to propapps in the unevaluated
;; propspec sets any new hostattrs, even though it's not meant to

(defmacro deploy (connections host &body additional-properties)
  "Establish CONNECTIONS to HOST, and apply each of the host's usual
properties, followed by specified by ADDITIONAL-PROPERTIES, an unevaluated
property application specification.

CONNECTION is a keyword identifying a connection type, a list beginning with
such a keyword and followed by keyword arguments required to establish the
connection, or a list of such lists.

Then HOST has all its usual static informational attributes, plus any set by
ADDITIONAL-PROPERTIES.  Static informational attributes set by
ADDITIONAL-PROPERTIES can override the host's usual static informational
attributes, in the same way that later entries in the list of properties
specified in DEFHOST forms can override earlier entries (see DEFHOST's
docstring).

The evaluation of ADDITIONAL-PROPERTIES to produce a property application
specification may retrieve existing hostattrs, but should not set any new
ones (not to be confused with how the :HOSTATTRS subroutines of properties in
ADDITIONAL-PROPERTIES may set additional hostattrs)."
  (once-only (host)
    `(deploy* ',connections
              ,host
              (let ((*host* (shallow-copy-host ,host)))
                (make-propspec
                 :propspec (props eseqprops ,@additional-properties))))))

(defmacro deploy-these (connections host &body properties)
  "Like DEPLOY, except apply each of the properties specified by PROPERTIES,
and not the host's usual properties, unless they also appear in PROPERTIES.
PROPERTIES is an unevaluated property application specification.

This function is useful to apply one or two properties to a host right now,
e.g. at the REPL when when testing new property definitions.  If HOST is
usually deployed using a Lisp-type connection, and the property you are testing
is :POSIX, you might use a connection type like :SSH so that you can quickly
alternate between redefining your work-in-progress property and seeing what
happens when you apply it to HOST.

HOST has all its usual static informational attributes, as set by its usual
properties, plus any set by PROPERTIES.  Static informational attributes set
by PROPERTIES can override the host's usual static informational attributes,
in the same way that later entries in the list of properties specified in
DEFHOST forms can override earlier entries (see DEFHOST's docstring).

The evaluation of PROPERTIES to produce a property application specification
may retrieve existing hostattrs, but should not set any new ones (not to be
confused with how the :HOSTATTRS subroutines of properties in PROPERTIES may
set additional hostattrs)."
  (once-only ((host `(ensure-host ,host)))
    `(deploy-these* ',connections
                    ,host
                    (let ((*host* (shallow-copy-host ,host)))
                      (make-propspec
                       :propspec (props eseqprops ,@properties))))))

(defmacro defdeploy (name (connections host) &body additional-properties)
  "Define a function which does (DEPLOY CONNECTIONS HOST ADDITIONAL-PROPERTIES).
You can then eval (NAME) to execute this deployment."
  `(defun ,name ()
     (deploy ,connections ,host ,@additional-properties)))

(defmacro defdeploy-these (name (connections host) &body properties)
  "Define a function which does (DEPLOY-THESE CONNECTIONS HOST PROPERTIES).
You can then eval (NAME) to execute this deployment."
  `(defun ,name ()
     (deploy-these ,connections ,host ,@properties)))

(defun hostdeploy* (host &optional additional-properties)
  "Like DEPLOY*, but use the host's default deployment."
  (deploy* (or (host-deployment host)
               (simple-program-error "Host has no default deployment"))
           host
           additional-properties))

(defun hostdeploy-these* (host properties)
  "Like DEPLOY-THESE*, but use the host's default deployment."
  (deploy-these* (or (host-deployment host)
                     (simple-program-error "Host has no default deployment"))
                 host
                 properties))

(defmacro hostdeploy (host &body additional-properties)
  "Like DEPLOY, but use the host's default deployment."
  (once-only (host)
    `(hostdeploy* ,host
                  (let ((*host* (shallow-copy-host ,host)))
                    (make-propspec
                     :propspec (props eseqprops ,@additional-properties))))))

(defmacro hostdeploy-these (host &body properties)
  "Like DEPLOY-THESE, but use the host's default deployment."
  (once-only (host)
    `(hostdeploy-these* ,host
                        (let ((*host* (shallow-copy-host ,host)))
                          (make-propspec
                           :propspec (props eseqprops ,@properties))))))

(defun hostname-f ()
  (stripln (run-program '("hostname" "-f") :output :string)))

(defmacro localsudo (&rest properties)
  "Deploy PROPERTIES to localhost using a :SUDO connection.

It is assumed that on this system the shell command 'hostname -f' will return
the full hostname, and that sudo is configured to ask for a password.  Useful
for testing properties at the REPL.  See also EVALS."
  (with-gensyms (username hostname host)
    `(let* ((,username (parse-username-from-id
                        (run-program '("id") :output :string)))
            (,hostname (hostname-f))
            (,host (or (symbol-value (find-symbol (string-upcase ,hostname)))
                       (make-host :hostattrs `(:hostname (,,hostname))
                                  :propspec (make-propspec :systems nil)))))
       (deploy-these*
        `((:sudo :as ,(format nil "~A@~A" ,username ,hostname)))
        ,host
        (let ((*host* (shallow-copy-host ,host)))
          (make-propspec :propspec (props eseqprops ,@properties)))))))

(defmacro localhd (&rest properties)
  "Deploy PROPERTIES to localhost using HOSTDEPLOY-THESE*.

It is assumed that on this system the shell command 'hostname -f' will return
the full hostname.  Useful for testing properties at the REPL.  See also
EVALS."
  (with-gensyms (hostname host)
    `(let* ((,hostname (hostname-f))
            (,host (or (symbol-value (find-symbol (string-upcase ,hostname)))
                       (error "Localhost not defined using DEFHOST?"))))
       (hostdeploy-these*
        ,host
        (let ((*host* (shallow-copy-host ,host)))
          (make-propspec :propspec (props eseqprops ,@properties)))))))

(defprop deploys :posix (connections host &optional additional-properties)
  "Execute the deployment which is defined by the pair (CONNECTIONS . HOST),
except possibly with the property application specification
ADDITIONAL-PROPERTIES also applied to HOST, like DEPLOY.

Useful to have one host act a controller, applying properties to other hosts.
Also useful to set up VMs, chroots, disk images etc. on localhost."
  (:desc (declare (ignore connections host additional-properties))
         "Subdeployment")
  (:preprocess
   (list (preprocess-connections connections)
         (preprocess-host
          (if additional-properties
              (union-propspec-into-host host additional-properties)
              host))))
  (:hostattrs
   (declare (ignore connections additional-properties))
   (%propagate-hostattrs host))
  (:apply
   (declare (ignore additional-properties))
   (%consfigure connections host)))

(defprop deploys-these :posix (connections host properties)
  "Like DEPLOYS, except apply to HOST each of the properties specified by
PROPERTIES, and not the host's usual properties, unless they also appear in
PROPERTIES, like DEPLOY-THESE."
  (:desc (declare (ignore connections host properties)) "Subdeployment")
  (:preprocess
   (list (preprocess-connections connections)
         (preprocess-host
          (replace-propspec-into-host (ensure-host host) properties))
         nil))
  (:hostattrs
   (declare (ignore connections properties))
   (%propagate-hostattrs host))
  (:apply
   (declare (ignore properties))
   (%consfigure connections host)))

(defprop reconnects :posix (connections properties)
  "Connect back to the same host with CONNECTIONS and apply PROPERTIES.
Mainly useful for using a connection type like :AS to apply properties as a
different user."
  (:desc (declare (ignore properties))
         (format nil "~S reconnection" connections))
  (:preprocess
   (list (preprocess-connections connections)
         (list :host nil :propspec properties)))
  (:hostattrs
   (declare (ignore connections))
   ;; Any hostattr set by PROPERTIES needs propagating upwards to *HOST*, but
   ;; the :DATA hostattrs set by PROPERTIES should be the only data that gets
   ;; propagated when establishing CONNECTIONS.  This ensures that for a
   ;; connection type like :SETUID, we don't copy all the prerequisite data
   ;; root has for the whole host into a user's homedir.
   ;;
   ;; To achieve this we reset the entry for :DATA, run the hostattrs
   ;; subroutines via PREPROCESS-HOST, and then manually propagate any new
   ;; hostattrs upwards.
   (let ((host (make-host :hostattrs (copy-list (hostattrs *host*))
                          :propspec (getf properties :propspec))))
     (setf (getf (slot-value host 'hostattrs) :data) nil)
     (setq host (preprocess-host host))
     (doplist (k v (hostattrs host))
       (loop with root = (get-hostattrs k)
             for cell on v until (eq cell root)
             collect (car cell) into accum
             finally (if (eql k :data)
                         (pushnew-hostattrs :data (nreverse accum))
                         (apply #'push-hostattrs k (nreverse accum)))))
     (dolist (system (propspec-systems (host-propspec host)))
       (pushnew system (slot-value (host-propspec *host*) 'systems)))
     (setf (getf properties :host) host)))
  (:apply
   (%consfigure connections (getf properties :host))))

(defun preprocess-connections (connections)
  (loop for connection in (ensure-cons connections)
        collect (apply #'preprocess-connection-args
                       (ensure-cons connection))))

(defun %propagate-hostattrs (host)
  (dolist (system (propspec-systems (host-propspec host)))
    (pushnew system (slot-value (host-propspec *host*) 'systems)))
  (pushnew-hostattrs :data (get-hostattrs :data host)))

(defprop evals :posix (&rest forms)
  "Property which just evaluates each of FORMS using EVAL.  Only for testing
newly defined functions and programmatic applications of properties at the
REPL with DEPLOY-THESE/HOSTDEPLOY-THESE -- do not add to hosts.

For example, to sudo to root to test your new function which needs root
privileges to do anything at all,

    (deploy-these :sudo melete.silentflame.com (evals '(my-new-function)))

where melete.silentflame.com is your laptop.

Note that while this property is declared to be :POSIX for flexibility,
whether it is actually :POSIX depends on what input and output FORMS perform."
  (:desc (format nil "Evaluated ~{~S~^ ~}" forms))
  (:apply (eval `(progn ,@forms))))
