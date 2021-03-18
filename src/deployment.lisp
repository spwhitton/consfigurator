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

;;;; Deployments

(defun %consfigure (connections host)
  "Configurator's primary loop, recursively binding *CONNECTION* and *HOST*.

Assumes HOST has already had its :HOSTATTRS subroutines run, and arguments to
connections in CONNECTIONS have been both normalised and preprocessed."
  (labels
      ((apply-propspec (propspec)
	 (let ((propapp (eval-propspec propspec)))
	   (assert-connection-supports (propapptype propapp))
	   (propappapply propapp)))
       (connect (connections)
	 (destructuring-bind ((type . args) . remaining) connections
	   ;; implementations of ESTABLISH-CONNECTION which call
	   ;; CONTINUE-DEPLOY* or CONTINUE-DEPLOY*-PROGRAM return nil to us
	   (when-let ((*connection*
		       (apply #'establish-connection type remaining args)))
	     (if remaining
		 (connect remaining)
		 (apply-propspec (host-propspec *host*)))
	     (connection-teardown *connection*)))))
    (let ((*host* (preprocess-host host)))
      (connect (if (eq :local (caar connections))
		   connections
		   (cons '(:local) connections))))))

(defun deploy* (connections host &optional additional-properties)
  "Execute the deployment which is defined by the pair (CONNECTIONS . HOST),
except possibly with the property application specification
ADDITIONAL-PROPERTIES also applied to HOST.

This is the entry point to Consfigurator's primary loop.  Typically users use
DEPLOY, DEPLOY-THESE, and the function definitions established by DEFDEPLOY,
DEFDEPLOY-THESE, etc., rather than calling this function directly.  However,
code which programmatically constructs deployments will need to call this."
  (%consfigure (preprocess-connections connections)
	       (if additional-properties
		   (%union-propspec-into-host (shallow-copy-host host)
					      additional-properties)
		   host)))

(defun deploy-these* (connections host &optional properties)
  "Like DEPLOY*, but replace the properties of HOST with PROPERTIES.

HOST has all its usual static informational attributes, as set by its usual
properties, plus any set by PROPERTIES.  Static informational attributes set
by PROPERTIES can override the host's usual static informational attributes,
in the same way that later entries in the list of properties specified in
DEFHOST forms can override earlier entries (see DEFHOST's docstring)."
  (%consfigure (preprocess-connections connections)
	       (if properties
		   (%replace-propspec-into-host (shallow-copy-host host)
						properties)
		   host)))

(defun continue-deploy* (remaining-connections)
  "Complete the work of an enclosing call to DEPLOY* or DEPLOY-THESE*.

Used by implementations of ESTABLISH-CONNECTION which need to do something
like fork(2) and then return to Consfigurator's primary loop in the child."
  (%consfigure remaining-connections *host*))

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
		(props eseqprops ,@additional-properties)))))

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
  (once-only (host)
    `(deploy-these* ',connections
		    ,host
		    (let ((*host* (shallow-copy-host ,host)))
		      (props eseqprops ,@properties)))))

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

(defprop deploys :posix (connections host &optional additional-properties)
  "Execute the deployment which is defined by the pair (CONNECTIONS . HOST),
except possibly with the property application specification
ADDITIONAL-PROPERTIES also applied to HOST, like DEPLOY.

Useful to have one host act a controller, applying properties to other hosts.
Also useful to set up VMs, chroots, disk images etc. on localhost."
  (:preprocess
   (list (preprocess-connections connections)
	 (preprocess-host
	  (if additional-properties
	      (%union-propspec-into-host (shallow-copy-host host)
					 additional-properties)
	      host))))
  (:hostattrs
   (declare (ignore connections additional-properties))
   (%propagate-hostattrs host))
  (:apply
   (declare (ignore additional-properties))
   (%consfigure connections host)))

(defprop deploys-these :posix (connections host &optional properties)
  "Like DEPLOYS, except apply to HOST each of the properties specified by
PROPERTIES, and not the host's usual properties, unless they also appear in
PROPERTIES, like DEPLOY-THESE."
  (:preprocess
   (list (preprocess-connections connections)
	 (preprocess-host
	  (%replace-propspec-into-host (shallow-copy-host host) properties))))
  (:hostattrs
   (declare (ignore connections properties))
   (%propagate-hostattrs host))
  (:apply
   (declare (ignore properties))
   (%consfigure connections host)))

(defun preprocess-connections (connections)
  (loop for connection in (ensure-cons connections)
	collect (apply #'preprocess-connection-args
		       (ensure-cons connection))))

(defun %propagate-hostattrs (host)
  (dolist (system (propspec-systems (host-propspec host)))
    (pushnew system (slot-value (host-propspec *host*) 'systems)))
  (dolist (attr (getf (hostattrs host) :data))
    (push-hostattrs :data attr)))
