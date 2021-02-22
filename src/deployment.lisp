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

(defmacro defdeploy (name (connection host) &body additional-properties)
  "Define a function which does (DEPLOY CONNECTION HOST ADDITIONAL-PROPERTIES).
You can then eval (NAME) to execute this deployment."
  `(defun ,name ()
     (deploy ,connection ,host ,@additional-properties)))

(defmacro defdeploy-these (name (connection host) &body properties)
  "Define a function which does (DEPLOY-THESE CONNECTION HOST PROPERTIES).
You can then eval (NAME) to execute this deployment."
  `(defun ,name ()
     (deploy-these ,connection ,host ,@properties)))

(defmacro defhostdeploy (connection host-name)
  "Where HOST-NAME names a host as defined with DEFHOST, define a function
which does (deploy CONNECTION (symbol-value HOST)).
You can then eval (HOST-NAME) to execute this deployment.

For example, if you usually deploy properties to athena by SSH,

    (defhost athena.silentflame.com
      (foo)
      (bar)
      ...)

    (defhostdeploy :ssh athena.silentflame.com)

and then you can eval (athena.silentflame.com) to apply athena's properties."
  `(defdeploy ,host-name (,connection ,host-name)))

;; this exists just to avoid exposing *HOST* but otherwise it's not really a
;; nice abstraction
(defun deploy*-form-for-remote-lisp (remaining)
  `(deploy* ,(or remaining :local) *host*))

(defmacro deploy (connection host &body additional-properties)
  "Establish a connection of type CONNECTION to HOST, and apply each of the
host's usual properties, followed by specified by ADDITIONAL-PROPERTIES, an
unevaluated property application specification.

CONNECTION is either a keyword identifying a connection type, or a list
beginning with such a keyword and followed by keyword arguments required to
establish the connection.

Then HOST has all its usual static informational attributes, plus any set by
ADDITIONAL-PROPERTIES.  Static informational attributes set by
ADDITIONAL-PROPERTIES can override the host's usual static informational
attributes, in the same way that later entries in the list of properties
specified in DEFHOST forms can override earlier entries (see DEFHOST's
docstring)."
  (once-only (host)
    (with-gensyms (propspec new-host)
      `(let* ((,propspec ,(props additional-properties))
	      (,new-host
		(make-instance 'host
			       :attrs (copy-list (slot-value ,host 'hostattrs))
			       :props (append-propspecs
				       (slot-value ,host 'propspec)
				       ,propspec))))
	 (let ((*host* ,new-host))
	   (eval-propspec-hostattrs ,propspec))
	 (deploy* ,connection ,new-host)))))

(defmacro deploy-these (connection host &body properties)
  "Establish a connection of type CONNECTION to HOST, and apply each of
the properties specified by PROPERTIES, an unevaluated property application
specification (and not the host's usual properties, unless they also appear
in PROPERTIES).

CONNECTION is either a keyword identifying a connection type, or a list
beginning with such a keyword and followed by keyword arguments required to
establish the connection.

This function is useful to apply one or two properties to a host right now,
e.g. at the REPL when when testing new property definitions.  If HOST is
usually deployed using a :lisp connection, and the property you are testing
is :posix, you might use a connection type like :ssh so that you can quickly
alternate between redefining your work-in-progress property and attempting to
apply it to HOST.

HOST has all its usual static informational attributes, as set by its usual
properties, plus any set by PROPERTIES.  Static informational attributes set
by PROPERTIES can override the host's usual static informational attributes,
in the same way that later entries in the list of properties specified in
DEFHOST forms can override earlier entries (see DEFHOST's docstring)."
  (with-gensyms (propspec new-host)
    `(let* ((,propspec ,(props properties))
	    (,new-host (make-instance 'host
				      :attrs (copy-list
					      (slot-value ,host 'hostattrs))
				      :props ,propspec)))
       (let ((*host* ,new-host))
	 (eval-propspec-hostattrs ,propspec))
       (deploy* ,connection ,new-host))))

(defun deploy* (connections host)
  ;; make a partial own-copy of HOST so that connections can add new pieces of
  ;; required prerequisite data; specifically, so that they can request the
  ;; source code of ASDF systems
  (let ((*host* (make-instance 'host
			       :attrs (copy-list (slot-value host 'hostattrs))
			       :props (slot-value host 'propspec))))
    (labels
	((connect (connections)
	   (destructuring-bind ((type . args) . remaining) connections
	     (when-let ((*connection*
			 (apply #'establish-connection type remaining args)))
	       (if remaining
		   (connect remaining)
		   (apply-propspec (slot-value *host* 'propspec)))
	       (connection-teardown *connection*))))
	 (apply-propspec (propspec)
	   (when (and (subtypep (class-of *connection*) 'posix-connection)
		      (eq :lisp (propspec->type propspec)))
	     (error "Cannot apply :lisp properties using :posix connection"))
	   (eval-propspec propspec)))
      (connect (loop for connection in (ensure-cons connections)
		     collect (mapcar #'preprocess-connection-args
				     (ensure-cons connection)))))))

;; these might need to be special-cased in parsing propspecs, because we
;; probably want it to be easy for the user to pass unevaluated propspecs to
;; these, but we want the evaluation to happen in the root Lisp.
;;
;; also, :HOSTATTRS subroutines of these will want to call
;; PREPROCESS-CONNECTION-ARGS in order to substitute in any values from
;; prerequisite data as early as possible
;;
;; One possibility is to allow :hostattrs subroutines to modify the arguments
;; which will get passed to the other routines, by giving them a special var
;; bound to the current propapp.  Then they could apply
;; preprocess-connection-args to the arguments.  (This suggests that deploy*
;; becomes simply applying the DEPLOYS property to the root Lisp, hrm.)
;;
;; (defprop deploys :posix (connection host &rest additional-properties)
;;   "Execute a Consfigurator deployment.
;;
;; Useful to have one host act a controller, applying properties to other hosts.
;; Also useful to set up VMs, chroots, disk images etc. on localhost.")
;;
;; (defprop deploys-these :posix (connection host &rest properties)
;;   "Execute a deployment, but replace the properties of host with PROPERTIES.
;; This property is to the DEPLOYS property what the DEPLOY-THESE function is to
;; the DEPLOY function.")
