;;; Consfigurator -- Lisp declarative configuration management system

;;; Copyright (C) 2018, 2021  Sean Whitton <spwhitton@spwhitton.name>

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

(in-package :consfigurator.property.libvirt)
(named-readtables:in-readtable :consfigurator)

(defproplist installed :posix ()
  "Install basic libvirt components."
  (:desc "libvirt installed")
  (os:etypecase
    (debianlike (apt:installed "libvirt-clients" "virtinst"
                               "libvirt-daemon" "libvirt-daemon-system"))))

(defprop %default-network-started :posix ()
  (:check
   (member "default" (mapcar #'car (virsh-get-columns "net-list"))
           :test #'string=))
  (:apply
   (mrun "virsh" "net-start" "default")))

(defprop %default-network-autostarted :posix ()
  (:check
   (remote-exists-p "/etc/libvirt/qemu/networks/autostart/default.xml"))
  (:apply
   (mrun "virsh" "net-autostart" "default")))

(defproplist default-network-started :posix ()
  "Ensure that the default libvirt network is started."
  (:desc "libvirt's default network started")
  (installed)
  (%default-network-started))

(defproplist default-network-autostarted :posix ()
  "Ensure that the default libvirt network is set to autostart, and start it.
On Debian, it is not started by default after installation of libvirt."
  (installed)
  (%default-network-autostarted)
  (%default-network-started))

(defmethod os-variant ((os os:debian-stable))
  (switch ((os:debian-suite os) :test #'string=)
    ("jessie" "debian8")
    ("stretch" "debian9")
    (t nil)))

(defmethod os-variant ((os os:debian-testing))
  "debiantesting")

(defmethod os-variant ((os os:debian-unstable))
  "debiantesting")

(defmethod os-variant (os))

(defprop defined :posix (host &rest arguments)
  "Define a libvirt domain for HOST by providing ARGUMENTS to virt-install(1).
With the current implementation, if ARGUMENTS changes, virt-install(1) will
not be run again.  You will need to either unapply and reapply this property,
or use virt-xml(1) to perform a modification.

Unapplying this property when the domain is running will use the 'undefine'
subcommand of virsh(1) to convert the running domain into a transient domain."
  (:check (declare (ignore arguments))
          (remote-exists-p (merge-pathnames (strcat (get-hostname host) ".xml")
                                            "/etc/libvirt/qemu/")))
  (:apply
   (with-remote-temporary-file (file)
     (mrun
      (format
       nil
       "virt-install --print-xml -n ~A~:[~; --os-variant=~:*~A~]~{ ~A~} >~S"
       (get-hostname host) (os-variant host)
       (mapcar #'escape-sh-token arguments) file))
     (mrun "virsh" "define" file)))
  (:unapply
   (declare (ignore arguments))
   (mrun "virsh" "undefine" (get-hostname host))))

(defun virsh-get-columns (&rest arguments)
  "Run a virsh command that is expected to yield tabular output, with the given
list of ARGUMENTS, and return the rows."
  (mapcar (lambda (row)
            (delete "" (split-string row) :test #'string=))
          (cddr (nbutlast (runlines "virsh" arguments)))))
