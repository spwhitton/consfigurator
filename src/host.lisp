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

;;;; Hosts

;; note that we expect any host object to be such that the :HOSTATTRS
;; subroutines of its propspec has already been run.  so, run them when
;; instantiating a new object, as DEFHOST does.
(defclass host ()
  ((hostattrs
    :initarg :attrs
    :reader hostattrs
    :documentation "Plist of the host's static informational attributes.")
   (propspec
    :initarg :props
    :reader host-propspec
    :documentation "Property application specification of the properties to
be applied to the host.")))

(defun make-host (&key hostattrs props)
  (let ((host (make-instance 'host :attrs hostattrs :props props)))
    (%eval-propspec-hostattrs host props)
    host))

(defmethod print-object ((host host) stream)
  (format stream "#.~S" `(make-instance
			  'host
			  :attrs ',(slot-value host 'hostattrs)
			  :props ,(slot-value host 'propspec)))
  host)

(defmethod %eval-propspec-hostattrs ((host host) (propspec propspec))
  "Modify HOST in-place according to :HOSTATTRS subroutines."
  (loop with *host* = host
	for form in (propspec-props propspec)
	for propapp = (compile-propapp form)
	do (propappattrs propapp)))

;; return values of the following two functions share structure, and thus are
;; not safe to use except on host objects that were just made, or that are
;; going straight into %CONSFIGURE

(defmethod %union-propspec-into-host ((host host) (propspec propspec))
  (prog1
      (setq host (make-instance 'host
				:attrs (hostattrs host)
				:props (append-propspecs (host-propspec host)
							 propspec)))
    (%eval-propspec-hostattrs host propspec)))

(defmethod %replace-propspec-into-host ((host host) (propspec propspec))
  (prog1
      (setq host (make-instance 'host
				:attrs (hostattrs host) :props propspec))
    (%eval-propspec-hostattrs host propspec)))

(defmacro defhost (hostname (&key deploy) &body properties)
  "Define a host with hostname HOSTNAME and properties PROPERTIES.
HOSTNAME can be a string or a symbol.  In either case, the host will get a
static informational property with its hostname as a string, and the symbol
whose name is the hostname will be bound to the host object.

DEPLOY represents the usual way you'll connect to the host to deploy
properties, and if specified, a function named HOSTNAME will be defined to
deploy the host using that connection chain.  This is an optional convenience
feature; you can always use DEPLOY and DEPLOY-THESE to apply properties to the
host using an arbitrary chain of connections.

If the first entry in PROPERTIES is a string, it will be considered a
human-readable description of the host.  Otherwise, PROPERTIES is an
unevaluated property application specification.  Recall that for atomic
entries (PROPERTY . ARGS), PROPERTY refers to the property that symbol names
in the global environment, not whatever it may name in the current dynamic
and/or lexical environments.  Property application specifications cannot
close over globally anonymous properties.

The order of PROPERTIES matters: deployments will apply properties to the host
in the order specified here, so later properties implicitly depend on earlier
ones.  In addition, static informational attributes set by later properties
are allowed to override any attributes with the same name set by earlier
entries."
  (let (hostname-sym attrs)
    (etypecase hostname
      (string (setq hostname-sym (intern hostname)))
      (symbol (setq hostname-sym hostname
		    hostname (string-downcase (symbol-name hostname)))))
    (push hostname (getf attrs :hostname))
    (when (stringp (car properties))
      (push (pop properties) (getf attrs :desc)))
    `(progn
       (declaim (type host ,hostname-sym))
       (defparameter ,hostname-sym
	 (%replace-propspec-into-host (make-instance 'host :attrs ',attrs)
				      ,(props properties))
	 ,(car (getf attrs :desc)))
       ,@(and deploy
	      `((defdeploy ,hostname-sym (,deploy ,hostname-sym)))))))
