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
;;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

(in-package :consfigurator.property.libvirt)
(named-readtables:in-readtable :consfigurator)

(defproplist installed :posix ()
  "Install basic libvirt components."
  (:desc "libvirt installed")
  (os:etypecase
    (debianlike (apt:installed "libvirt-clients" "virtinst"
                               "libvirt-daemon" "libvirt-daemon-system"))))

(defprop %default-network-started :posix ()
  (:desc "libvirt's default network started")
  (:check
   (member "default" (mapcar #'car (virsh-get-columns "net-list"))
           :test #'string=))
  (:apply
   (mrun "virsh" "net-start" "default")))

(defprop %default-network-autostarted :posix ()
  (:desc "libvirt's default network marked for autostart")
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
  (:desc "libvirt's default network started & marked for autostart")
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

(defprop defined-for :posix (host &rest arguments)
  "Define a libvirt domain for HOST by providing ARGUMENTS to virt-install(1).
With the current implementation, if ARGUMENTS changes, virt-install(1) will
not be run again.  You will need to either unapply and reapply this property,
or use virt-xml(1) to perform a modification.

Unapplying this property when the domain is running will use the 'undefine'
subcommand of virsh(1) to convert the running domain into a transient domain."
  (:desc "libvirt domain XML defined")
  (:check (declare (ignore arguments))
          (remote-exists-p (merge-pathnames (strcat (get-hostname host) ".xml")
                                            "/etc/libvirt/qemu/")))
  (:apply
   (with-remote-temporary-file (file)
     (mrun
      (format
       nil
       "virt-install --print-xml -n ~A --osinfo=~A~{ ~A~} >~S"
       (get-hostname host)
       (or (os-variant (get-hostattrs-car :os host)) "detect=on,require=off")
       (mapcar #'sh-escape arguments) file))
     (mrun "virsh" "define" file)))
  (:unapply
   (declare (ignore arguments))
   (mrun "virsh" "undefine" (get-hostname host))))

(defprop started :posix (host)
  "Ensure the libvirt domain for the host designated by HOST is started.
(I.e., if HOST is a string, ensure the domain named HOST is started; if HOST
is a HOST value, start the libvirt domain whose name is HOST's hostname.)"
  (:desc #?"libvirt domain ${(get-hostname host)} started")
  (:check (or (service:no-services-p) (host-domain-started-p host)))
  (:apply (mrun "virsh" "start" (get-hostname host))))

(defprop destroyed :posix (host)
  "Ensure the libvirt domain for the host designated by HOST is destroyed."
  (:desc #?"libvirt domain ${(get-hostname host)} destroyed")
  (:check (not (host-domain-started-p host)))
  (:apply (mrun "virsh" "destroy" (get-hostname host))))

(defmacro when-started (host &body propapps)
  "Apply PROPAPPS only when the libvirt domain for the host designated by HOST
is already running.
Useful to conditionalise a DEPLOYS property to do nothing unless the VM is
already running, for a VM which is not always booted, e.g. on a laptop."
  `(when-started*
    ',host
    ,(if (cdr propapps) `(eseqprops ,@propapps) (car propapps))))

(define-function-property-combinator when-started* (host propapp)
  (macrolet ((check-started (form)
               `(if (host-domain-started-p host)
                    ,form :no-change)))
    (:retprop :type (propapp-type propapp)
              :desc (get (car propapp) 'desc)
              :hostattrs (get (car propapp) 'hostattrs)
              :apply (lambda-ignoring-args
                       (check-started (apply-propapp propapp)))
              :unapply (lambda-ignoring-args
                         (check-started (unapply-propapp propapp)))
              :args (cdr propapp))))

;; Another possible approach would be to convert DISK:VOLUME values to --disk
;; arguments to virt-install(1).

(defpropspec kvm-boots-chroot-for :lisp
    (options host &optional additional-properties
             &aux (host* (preprocess-host
                          (make-child-host
                           :hostattrs (hostattrs host)
                           :propspec (host-propspec
                                      (union-propspec-into-host
                                       host additional-properties))))))
  "Build a chroot for HOST and boot it as a libvirt KVM virtual machine.
Virtio-FS and direct kernel boot are used to avoid the need for either a
bootloader or an intermediary disk image.  That makes this property suitable
for both long-lived virtual machines and quickly bringing up fresh OS installs
for testing.

OPTIONS is a plist of keyword parameters.

  - :VCPUS -- corresponds to the --vcpus argument to virt-install(1).
    Defaults to 1.

  - :MEMORY -- corresponds to the --memory argument to virt-install(1).
    Defaults to 1024.

  - :AUTOSTART -- Lisp boolean corresponding to the --autostart argument to
    virt-install(1), and also determines whether applying this property
    attempts to start the VM.  Defaults to nil.

  - :VIRT-OPTIONS -- list of additional arguments to pass to virt-install(1).

  - :CHROOT-OPTIONS -- passed on to CHROOT:OS-BOOTSTRAPPED-FOR, which see.

  - :ALWAYS-DEPLOYS -- Thanks to Virtio-FS, the VM's root filesystem is
    simultaneously accessible to both the hypervisor host and the VM.  That
    means the chroot can be updated by Consfigurator even while the VM is
    running.  If this parameter is true then each time this property is
    applied, the chroot will be updated to reflect any changes to HOST and
    ADDITIONAL-PROPERTIES, using DEPLOYS.  This has the advantage that you do
    not need to arrange connecting to the VM to keep its configuration
    up-to-date.

    It is not appropriate to set this for untrusted VMs because it could be
    used to break out into the hypervisor.  However, even for VMs which will
    handle untrusted data you could temporarily set this while incrementally
    building up the list of properties the VM will need, before it has been
    exposed to anything untrusted.

    Some properties do nothing or do not have their full effect when applied
    to a chroot, such as properties which start services.  If you have
    properties like that, and/or the VM is untrusted, leave this parameter set
    to nil, and arrange deploying the host by connecting to the running VM,
    perhaps by adding a call to DEPLOYS to the definition of the hypervisor
    host (see example below).

    Defaults to nil.

  - :KERNEL -- Path to the kernel, under the chroot if relative.  Defaults to
    \"vmlinuz\".

  - :INITRD -- Path to the initrd, under the chroot if relative.  Defaults to
    \"initrd.img\".

  - :APPEND -- String to append to the kernel command line.

If the :VCPUS, :MEMORY, :AUTOSTART or :VIRT-OPTIONS parameters change,
virt-install(1) will not be rerun; see LIBVIRT:DEFINED-FOR.

Sample usage:

    (defhost subbox.laptop.example.com ()
      (os:debian-stable \"bullseye\" :amd64)
      (apt:installed \"linux-image-amd64\")

      (hostname:configured)
      (network:static \"ens4\"
       \"192.168.122.31\" \"192.168.122.1\" \"255.255.255.0\")
      (file:has-content \"/etc/resolv.conf\" \"...\")

      (sshd:installed)
      (as \"root\" (ssh:authorized-keys \"...\")))

    (defhost laptop.example.com ()
      ;; ...

      (file:contains-conf-tab \"/etc/hosts\"
                              \"192.168.122.31\" \"subbox.laptop.example.com\")
      (libvirt:kvm-boots-chroot-for. (:vcpus 2 :memory 2048)
          subbox.laptop.example.com
        ;; Apply TIMEZONE:CONFIGURED-FROM-PARENT here rather than in the
        ;; DEFHOST above because it can be applied only during a subdeployment
        ;; of laptop.example.com -- when we deploy subbox directly, there is
        ;; no parent host.
        (timezone:configured-from-parent))
      (libvirt:when-started subbox.laptop.example.com
        ;; Here we elide most of the configuration necessary to ensure that
        ;; root can SSH into the VM.  You can deploy a passwordless SSH key,
        ;; set up OpenSSH host-based authentication, or use SSH agent
        ;; forwarding across the connection chain by means of which you
        ;; deploy laptop.example.com.  This last option has the disadvantage
        ;; that you can't easily switch to deploying laptop.example.com with
        ;; other connection chains.
        ;;
        ;; For example, you can use (:sudo :sbcl) to deploy laptop.example.com
        ;; and also execute 'xhost +SI:localuser:root' to enable root's SSH
        ;; process to prompt you (the :SUDO connection type preserves
        ;; SSH_AUTH_SOCK when sudoing to root).  Another possibility is to use
        ;; :SETUID to switch back to the account which has your SSH agent in
        ;; the DEPLOYS propapp.
        (deploys ((:ssh :user \"root\") :sbcl) subbox.laptop.example.com)))

There's repetition here, and you might like to use DEFPROPSPEC to establish
your preferred VM networking setup and corresponding DEPLOYS propapp."
  (:desc #?"libvirt KVM VM for ${(get-hostname host*)} defined")
  ;; Same hostname probably means that the VM HOST inherited the hypervisor
  ;; HOST's hostname as one was not explicitly set; probably a mistake.
  (when (string= (get-hostname host*) (get-hostname))
    (failed-change "KVM VM has same hostname as hypervisor host."))
  (destructuring-bind
      (&key (vcpus 1) (memory 1024) autostart
         virt-options chroot-options always-deploys
         (kernel "vmlinuz") (initrd "initrd.img") append
       &aux (chroot (ensure-directory-pathname
                     (merge-pathnames (get-hostname host*)
                                      #P"/var/lib/libvirt/images/")))
         (flagfile (merge-pathnames
                    (strcat (get-hostname host*) ".bootstrapped")
                    #P"/var/lib/libvirt/images/"))
         (additional-properties
          (append-propspecs
           additional-properties
           (make-propspec
            :propspec
            '(os:etypecase
              (debianlike
               (apt:installed "initramfs-tools")
               (on-change (desc "virtiofs module added to initramfs"
                                (file:contains-lines
                                 "/etc/initramfs-tools/modules" "virtiofs"))
                 (cmd:single "update-initramfs" "-u"))))))))
      options
    `(with-unapply
       (installed)
       (file:contains-conf-equals "/etc/libvirt/qemu.conf"
                                  "memory_backing_dir" #?'"/dev/shm"')
       ,@(if always-deploys
             `((chroot:os-bootstrapped-for
                ,chroot-options ,chroot ,host ,additional-properties)
               ;; Create the flagfile anyway in case ALWAYS-DEPLOYS is
               ;; changed t->nil right after this deploy.
               (file:has-content ,flagfile ""))
             `((with-flagfile ,flagfile
                 (chroot:os-bootstrapped-for
                  ,chroot-options ,chroot ,host ,additional-properties))))
       (defined-for ,host*
           ,(format nil "--vcpus=~D" vcpus) ,(format nil "--memory=~D" memory)
         "--filesystem"
         ,(format
           nil
"type=mount,accessmode=passthrough,driver.type=virtiofs,source.dir=~A,target.dir=rootfs"
           chroot)
         "--boot"
         ,(format
           nil
"kernel=~A,initrd=~A,kernel_args=\"root=rootfs rootfstype=virtiofs rw~:[~; ~:*~A~]\""
           (ensure-pathname kernel :defaults chroot :ensure-absolute t)
           (ensure-pathname initrd :defaults chroot :ensure-absolute t)
           append)
         "--memorybacking=access.mode=shared"
         ,@(and autostart '("--autostart"))
         ,@virt-options)
       ,@(and autostart `((started ,host)))
       :unapply
       (destroyed ,host*)
       (unapplied (defined-for ,host*))
       (unapplied
        (with-flagfile ,flagfile
          (chroot:os-bootstrapped-for
           ,chroot-options ,chroot ,host ,additional-properties))))))

(defproplist kvm-boots-chroot :lisp (options properties)
  "Like LIBVIRT:KVM-BOOTS-CHROOT-FOR but define a new host using PROPERTIES."
  (:desc #?"libvirt KVM VM for chroot defined")
  (kvm-boots-chroot-for options (make-host :propspec properties)))

(defun virsh-get-columns (&rest arguments)
  "Run a virsh command that is expected to yield tabular output, with the given
list of ARGUMENTS, and return the rows."
  (mapcar #'words (cddr (nbutlast (runlines "virsh" arguments)))))

(defun host-domain-started-p (host)
  ;; The "State" column in the output of 'virsh list' is to be ignored here;
  ;; 'virsh start' will do nothing if the VM appears at all in the output of
  ;; 'virsh list'.
  (member (get-hostname host) (mapcar #'cadr (virsh-get-columns "list"))
          :test #'string=))
