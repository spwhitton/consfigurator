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

;;;; Hosts

(defclass host ()
  ((hostattrs
    :initarg :hostattrs
    :reader hostattrs
    :documentation "Plist of the host's static informational attributes.")
   (propspec
    :initarg :propspec
    :reader host-propspec
    :documentation "Propspec of the properties to be applied to the host.")
   (default-deployment
    :initform nil
    :initarg :deploy
    :reader host-deployment
    :documentation
    "Connection chain representing the usual way this host is deployed."))
  (:documentation "Abstract superclass for hosts.  Do not instantiate."))

(defclass preprocessed-host (host)
  ((propspec
    :type preprocessed-propspec))
  (:documentation
   "A host whose :PREPROCESS and :HOSTATTRS subroutines have been run."))

(defclass unpreprocessed-host (host)
  ((propspec
    :type unpreprocessed-propspec))
  (:documentation
   "A host whose :PREPROCESS and :HOSTATTRS subroutines have not been run."))

(defmethod shallow-copy-host ((host host))
  (make-instance (type-of host)
                 :hostattrs (copy-list (hostattrs host))
                 :propspec (host-propspec host)))

(defmacro with-preserve-hostattrs (&body forms)
  "Evaluate FORMS then throw away any newly added hostattrs.
Useful in property combinators when you need to run some :HOSTATTRS
subroutines but ignore any new hostattrs they may push.  Shouldn't be used in
properties."
  `(let ((*host* (shallow-copy-host *host*)))
     ,@forms))

(defgeneric preprocess-host (host)
  (:documentation
   "Convert a host into a fresh preprocessed host if necessary, and
unconditionally perform a shallow copy of the plist of static information
attributes, so that implementations of ESTABLISH-CONNECTION can push new
attributes (typically to request prerequisite data) without disturbing host
values higher up the call stack."))

(defmethod preprocess-host ((host preprocessed-host))
  (shallow-copy-host host))

(defmethod preprocess-host ((host unpreprocessed-host))
  (let ((*host* (make-instance
                 'preprocessed-host
                 :hostattrs (copy-list (hostattrs host))
                 :propspec (preprocess-propspec (host-propspec host)))))
    (propappattrs (eval-propspec (host-propspec *host*)))
    *host*))

(defun make-host (&key hostattrs (propspec (make-propspec)) deploy)
  (make-instance 'unpreprocessed-host
                 :hostattrs hostattrs :propspec propspec :deploy deploy))

(defun make-child-host (&key hostattrs propspec)
  "Make a host object to represent a chroot, container or the like.
Called by properties which set up such subhosts, like CHROOT:OS-BOOTSTRAPPED."
  (make-instance
   'unpreprocessed-host
   :propspec propspec
   :hostattrs (list* :parent-hostattrs (hostattrs *host*) hostattrs)))

(defmethod print-object ((host host) stream)
  (format stream "#.~S" `(make-instance
                          ',(type-of host)
                          :hostattrs ',(slot-value host 'hostattrs)
                          :propspec ,(slot-value host 'propspec)
                          :deploy ',(slot-value host 'default-deployment)))
  host)

(defmethod union-propspec-into-host
    ((host unpreprocessed-host) (propspec propspec))
  (make-instance 'unpreprocessed-host
                 :hostattrs (copy-list (hostattrs host))
                 :propspec (append-propspecs (host-propspec host) propspec)))

(defmethod replace-propspec-into-host
    ((host unpreprocessed-host) (propspec unpreprocessed-propspec))
  ;; we have to preprocess HOST as functions that call us want the return
  ;; value to have all the hostattrs it would have were PROPSPEC not to be
  ;; substituted in
  (make-instance 'unpreprocessed-host
                 :hostattrs (hostattrs
                             (preprocess-host (shallow-copy-host host)))
                 :propspec propspec))

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
         (make-host :hostattrs ',attrs
                    :propspec (make-propspec
                               :propspec (props seqprops ,@properties))
                    :deploy ',deploy)
         ,(car (getf attrs :desc)))
       ,@(and deploy
              `((defdeploy ,hostname-sym (,deploy ,hostname-sym)))))))
