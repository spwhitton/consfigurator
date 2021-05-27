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

(defun virsh-get-columns (&rest arguments)
  "Run a virsh command that is expected to yield tabular output, with the given
list of ARGUMENTS, and return the rows."
  (mapcar (lambda (row)
            (delete "" (split-string row) :test #'string=))
          (cddr (nbutlast (runlines "virsh" arguments)))))
