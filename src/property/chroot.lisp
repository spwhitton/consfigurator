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

(in-package :consfigurator.property.chroot)
(named-readtables:in-readtable :consfigurator)

(defprop %debootstrapped :posix (root host &rest options)
  "Bootstrap The Universal Operating System into ROOT using debootstrap(1)."
  (:check
   (declare (ignore options host))
   ;; check whether a previous debootstrap failed partway through
   (if (test "-d" (merge-pathnames "debootstrap/"
				   (ensure-directory-pathname root)))
       (progn (mrun "rm" "-rf" root) nil)
       (test "-d" root)))
  (:apply
   (let* ((os (car (getf (hostattrs host) :os)))
	  (args (list (if (os:supports-arch-p (get-hostattrs-car :os)
					      (os:linux-architecture os))
			  "debootstrap" "qemu-debootstrap")
		      (plist-to-cmd-args options)
		      (strcat "--arch=" (os:debian-architecture os))
		      (os:debian-suite os)
		      root)))
     (when-let ((proxy (get-hostattrs-car :apt.proxy)))
       (setq args (list* :env (list :http_proxy proxy) args)))
     (when-let ((mirror (get-hostattrs-car :apt.mirror)))
       (nconcf args (list mirror)))
     (apply #'run args))))

(defpropspec %os-bootstrapper-installed :posix (host)
  `(os:host-typecase ,host
     (debian
      (os:typecase
	(debian (apt:installed "debootstrap"))))))

(defpropspec %os-bootstrapped :posix (options root host)
  "Bootstrap OS into ROOT, e.g. with debootstrap(1)."
  ;; evaluate HOST once; can't use ONCE-ONLY because gensyms not serialisable
  ;; for sending to remote Lisp images
  (let ((host host))
    `(os:host-typecase ,host
       (debian (%debootstrapped ,root ,host ,@options)))))

(defproplist os-bootstrapped :posix
    (options root properties
	     &aux (host
		   (preprocess-host
		    (make-child-host
		     :propspec
		     (make-propspec
		      :systems (propspec-systems properties)
		      ;; Note that this apply-and-unapply approach will not
		      ;; work as part of a larger propspec because unapplying
		      ;; SERVICE:NO-SERVICES doesn't remove the :NO-SERVICES
		      ;; hostattr.  It's only okay because we know that the
		      ;; unapplication is the last property that will be
		      ;; applied to HOST (though perhaps not to the chroot).
		      :propspec `(eseqprops
				  (service:no-services)
				  ,(propspec-props properties)
				  (unapply (service:no-services))))))))
  "Bootstrap an OS into ROOT and apply PROPERTIES.
OPTIONS is a plist of values to pass to the OS-specific bootstrapping property."
  (:desc
   (declare (ignore options properties))
   #?"Built chroot ${root}")
  (%os-bootstrapper-installed host)
  (%os-bootstrapped options root host)
  (deploys `((:chroot :into ,root)) host))
