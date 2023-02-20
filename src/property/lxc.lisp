;;; Consfigurator -- Lisp declarative configuration management system

;;; Copyright (C) 2021, 2023  Sean Whitton <spwhitton@spwhitton.name>

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

(in-package :consfigurator.property.lxc)
(named-readtables:in-readtable :consfigurator)

;;;; Properties and combinators

(defproplist installed :posix ()
  "Install the LXC userspace tools."
  (:desc "LXC installed")
  (os:etypecase
    (debianlike (apt:installed "lxc"))))

(defmacro default-maps-params (uid-maps-param gid-maps-param)
  `(setq ,uid-maps-param
         (or ,uid-maps-param
             (list (cons 0 (multiple-value-list
                            (get-ids-offset "/etc/subuid" user)))))
         ,gid-maps-param
         (or ,gid-maps-param
             (list (cons 0 (multiple-value-list
                            (get-ids-offset "/etc/subgid" user)))))))

(defprop user-container-started :posix (host &optional owner)
  "Ensure the LXC unprivileged container for the host designated by HOST owned
by OWNER, defaulting to the current user, is started.
(I.e., if HOST is a string, ensure the container named HOST is started; if
HOST is a HOST value, start the container whose name is HOST's hostname.)"
  (:desc #?"LXC container ${(get-hostname host)} started")
  (:check (or (service:no-services-p) (user-container-active-p host owner)))
  (:apply (lxc-cmd owner "lxc-unpriv-start" "-n" (get-hostname host))))

(defprop user-container-stopped :posix (host &optional owner)
  "Ensure the LXC unprivileged container for the host designated by HOST owned
by OWNER, defaulting to the current user, is stopped."
  (:desc #?"LXC container ${(get-hostname host)} stopped")
  (:check (not (user-container-active-p host owner)))
  (:apply (lxc-cmd owner "lxc-stop" "-n" (get-hostname host))))

(defmacro when-user-container-running ((host &key owner) &body propapps)
  "Apply PROPAPPS only when the unprivileged LXC for the host designated by HOST
and owned by OWNER, defaulting to the current user, is already started."
  `(when-user-container-running*
    ,host ,owner
    ,(if (cdr propapps) `(eseqprops ,@propapps) (car propapps))))

(define-function-property-combinator
    when-user-container-running* (host owner propapp)
  (macrolet ((check-running (form)
               `(if (user-container-running-p host owner) ,form :no-change)))
    (:retprop :type (propapp-type propapp)
              :desc (get (car propapp) 'desc)
              :hostattrs (get (car propapp) 'hostattrs)
              :apply (lambda-ignoring-args
                       (check-running (apply-propapp propapp)))
              :unapply (lambda-ignoring-args
                         (check-running (unapply-propapp propapp)))
              :args (cdr propapp))))

(defproplist user-containers-autostart :posix (user)
  "Install a systemd user unit for USER to autostart all LXC user containers
owned by that user which have lxc.start.auto turned on.  Also ensures that
lingering is enabled for USER, so the user unit triggers at system boot.

A limitation of the current implementation is that it assumes XDG_CONFIG_HOME
is ~/.config."
  (:desc #?"LXC autostart systemd user unit installed for ${user}")
  (user:has-account user)
  (systemd:lingering-enabled user)
  (as user
    (file:has-content ".config/systemd/user/lxc-autostart.service"
      '("[Unit]"
        "Description=\"lxc-autostart\""

        "[Service]"
        "Type=oneshot"
        "Delegate=yes"
        "ExecStart=/usr/bin/lxc-autostart"
        "ExecStop=/usr/bin/lxc-autostart --shutdown"
        "RemainAfterExit=yes"

        "[Install]"
        "WantedBy=default.target"))
    (systemd:enabled "lxc-autostart" :user-instance t)))

(defprop usernet-veth-usable-by :posix
    (user &optional (interface "lxcbr0") (count 10))
  "Ensure that USER is allowed to attach up to COUNT unprivileged LXCs to the
LXC-managed bridge INTERFACE.

As a special case, INTERFACE may also be \"none\", which gives USER permission
to create veth pairs where the peer outside the container is not attached to
any bridge."
  (:desc #?"${user} may attach LXC veth devices to ${interface}")
  (:apply (file:map-remote-file-lines
           "/etc/lxc/lxc-usernet"
           (lambda (lines)
             (loop with done
                   and want = (format nil "~A veth ~A ~D" user interface count)
                   and prefix = (strcat user " veth " interface)
                   for line in lines
                   if (string-prefix-p prefix line)
                     unless done collect want into accum and do (setq done t)
                       end
                   else collect line into accum
                   finally (return
                             (if done accum (nconc accum (list want)))))))))

(defprop %ids-shifted-for :lisp
    (user directory uid-maps gid-maps
          &optional
          (rootfs
           (merge-pathnames "rootfs/"
                            (ensure-directory-pathname directory))))
  "Recursively shift the user and group ownership of ROOTFS according to
UID-MAPS and GID-MAPS and chown DIRECTORY to root's UID according to UID-MAPS.
Not idempotent!  Also set the mode of DIRECTORY to 0770, as is standard for
unprivileged LXCs."
  (:apply
   (default-maps-params uid-maps gid-maps)
   (let ((dir (ensure-directory-pathname directory))
         (uidmap (reduce-id-maps uid-maps))
         (gidmap (reduce-id-maps gid-maps)))
     (handler-bind ((serious-condition
                      ;; Don't leave a partially-shifted tree.
                      (lambda-ignoring-args (delete-remote-trees rootfs))))
       (shift-ids rootfs uidmap gidmap))
     ;; Don't see how to pass (gid_t)-1 as the third argument via CFFI.  Note
     ;; that gid_t is not guaranteed to be unsigned.
     (nix:chown dir (funcall uidmap 0) (nix:stat-gid (nix:stat dir)))
     (nix:chmod dir #o770))))

(defprop %container-config-populated :posix
    (prelude-lines user uid-maps gid-maps directory autostart hostname
                   additional-lines)
  (:apply
   (default-maps-params uid-maps gid-maps)
   (let ((uid-maps (loop for (inside outside count) in uid-maps
                         collect (format nil "lxc.idmap = u ~D ~D ~D"
                                         inside outside count)))
         (gid-maps (loop for (inside outside count) in gid-maps
                         collect (format nil "lxc.idmap = g ~D ~D ~D"
                                         inside outside count)))
         (rootfs
           (strcat
            "dir:"
            (unix-namestring
             (merge-pathnames
              "rootfs"
              (merge-pathnames directory (get-connattr :remote-home)))))))
     (file:has-content (merge-pathnames "config" directory)
       (append prelude-lines uid-maps gid-maps
               (list (strcat "lxc.rootfs.path = " rootfs)
                     (strcat "lxc.start.auto = " (if autostart "1" "0"))
                     (strcat "lxc.uts.name = " hostname))
               additional-lines)
       :mode #o640))))

(defpropspec user-container-for :lisp
    (options user host &optional additional-properties
             &aux (host* (preprocess-host
                          (make-child-host
                           :hostattrs (hostattrs host)
                           :propspec (host-propspec
                                      (union-propspec-into-host
                                       host additional-properties))))))
  "Build an unprivileged, non-system-wide LXC container for HOST.
Must be applied using a connection chain which grants root access, primarily
for the sake of bootstrapping the container's root filesystem.  Once built,
however, the container will be launched by USER, which should be non-root.

If the container has already been bootstrapped and is running at the time this
property is applied, enter the container and apply all its properties.

OPTIONS is a plist of keyword parameters:

  - :AUTOSTART -- Lisp boolean corresponding to lxc.start.auto in the
    container's config file, and also determines whether applying this
    property attempts to start the container.  Defaults to nil.  See also
    LXC:USER-CONTAINERS-AUTOSTART.

  - :PRELUDE-LINES -- additional lines to prepend to the container's
    configuration file, before the lines generated by this property.  See
    lxc.container.conf(5).  The default value is usually sufficient; if you
    add lines, you will probably want to include the lines from the default
    value too.

  - :ADDITIONAL-LINES -- additional lines to append to the container's
    configuration file, after the lines generated by this property.  See
    lxc.container.conf(5).  In most cases you will need to include, at a
    minimum, lines setting up a network interface for the container.  The
    default value serves as an example of a standard way to do this; if you
    use them unmodified, you will also need to apply
    LXC:USERNET-VETH-USABLE-BY for USER before this property.

  - :UID-MAPS -- a list of the form (INSIDE OUTSIDE COUNT), or a list of such
    lists, specifying the subordinate UIDs for the container's user namespace.
    OUTSIDE is the beginning of a UID range, as seen from outside the
    container, and INSIDE is the UID that OUTSIDE is mapped to, as seen from
    inside the container.  COUNT is the number of consecutive UIDs mapped.
    This property will ensure that USER has permission to use that range of
    UIDs by updating /etc/subuid if necessary.

    As a special case, if NIL, instead use the first range of UIDs assigned to
    USER in /etc/subuid, with a value of zero for INSIDE, and do not modify
    /etc/subuid.  (If you want to use the first range of UIDs assigned to USER
    in /etc/subuid and also other ranges, you must specify them all explicitly
    and cannot rely on this special case.)

    It is usually sufficient not to specify this parameter, as distribution
    scripts automatically add an entry to /etc/subuid for each regular user,
    and most containers use a value of zero for INSIDE.

  - :GID-MAPS -- as :UID-MAPS, but for GIDs and /etc/subgid.

  - :CHROOT-OPTIONS -- passed on to CHROOT:OS-BOOTSTRAPPED-FOR, which see.

A limitation of the current implementation is that the root filesystem of the
container is always created under ~/.local/share/lxc/HOSTNAME where HOSTNAME
is the hostname of HOST, ignoring any configured XDG_DATA_HOME for USER.

Internally we use setns(2) to enter the container.  See \"Connections which
use setns(2) to enter containers\" in the Consfigurator manual for security
implications."
  (:desc #?"LXC container for ${(get-hostname host*)} configured")
  ;; Same hostname probably means that the container HOST inherited the
  ;; container host's hostname as one was not explicitly set; probably a
  ;; mistake.
  (when (string= (get-hostname host*) (get-hostname))
    (aborted-change "LXC container has same hostname as container host."))
  (destructuring-bind
      (&key chroot-options autostart uid-maps gid-maps
         (prelude-lines '("lxc.include = /usr/share/lxc/config/common.conf"
                          "lxc.include = /usr/share/lxc/config/userns.conf"))
         (additional-lines '("lxc.net.0.type = veth"
                             "lxc.net.0.flags = up"
                             "lxc.net.0.link = lxcbr0"))
       &aux
         (directory
          (ensure-directory-pathname
           (merge-pathnames (get-hostname host*) ".local/share/lxc/")))
         (rootfs (merge-pathnames "rootfs/" directory))
         (uid-maps (if (listp (car uid-maps)) uid-maps (list uid-maps)))
         (gid-maps (if (listp (car gid-maps)) gid-maps (list gid-maps))))
      options
    `(eseqprops
      (installed)
      (user:has-account ,user)
      (systemd:lingering-enabled ,user) ; required for lxc-ls(1) to work at all
      ,@(aand (loop for (inside outside count) in uid-maps
                    collect (format nil "~A:~D:~D" user outside count))
              `((desc ,#?"/etc/subuid has mappings for ${(get-hostname host*)}"
                      (file:contains-lines "/etc/subuid" ,@it))))
      ,@(aand (loop for (inside outside count) in gid-maps
                    collect (format nil "~A:~D:~D" user outside count))
              `((desc ,#?"/etc/subgid has mappings for ${(get-hostname host*)}"
                      (file:contains-lines "/etc/subgid" ,@it))))
      ,(propapp (desc "Base directory for container exists"
                      (as user (file:directory-exists directory))))
      (with-homedir (:user ,user)
        (with-flagfile ,(merge-pathnames "rootfs.bootstrapped" directory)
          ;; It would be nice to branch here such that if we are about to
          ;; start up the container and enter it, just bootstrap a minimal
          ;; root filesystem, and only otherwise get all the other properties
          ;; applied before the ID shifting.  I.e.
          ;;
          ;;     (chroot:os-bootstrapped-for
          ;;      ,chroot-options ,rootfs
          ;;      ,@(if autostart
          ;;            `(,(make-host :hostattrs
          ;;                          (list :os (get-hostattrs :os host*))))
          ;;            `(,host ,additional-properties)))
          ;;
          ;; However, it might be that we need to apply the other properties
          ;; in order that the container is startable; for example, getting
          ;; systemd installed.
          (chroot:os-bootstrapped-for
           ,chroot-options ,rootfs ,host ,additional-properties)
          (%ids-shifted-for ,user ,directory ,uid-maps ,gid-maps)))
      ,(propapp
        (desc "Container configuration file populated"
              (as user
                (%container-config-populated
                 prelude-lines user uid-maps gid-maps directory autostart
                 (car (split-string (get-hostname host*) :separator "."))
                 additional-lines))))
      ,@(and autostart `((user-container-started ,host ,user)))
      (when-user-container-running (,host :owner ,user)
        (deploys ((:lxc :owner ,user :name ,(get-hostname host*)))
                 ,host ,additional-properties)))))

(defproplist user-container :lisp (options user properties)
  "Like LXC:USER-CONTAINER-FOR but define a new host using PROPERTIES."
  (:desc "LXC container defined")
  (user-container-for options user (make-host :propspec properties)))


;;;; Utility functions

(defun lxc-cmd (&optional owner &rest cmd-and-args)
  (let* ((runuser
           (and owner (not (string= owner (get-connattr :remote-user)))))
         (uid (if runuser
                  (user:passwd-field 2 owner)
                  (get-connattr :remote-uid))))
    (apply #'run :env `(:DBUS_SESSION_BUS_ADDRESS nil
                        :XDG_RUNTIME_DIR ,(format nil "/run/user/~D" uid))
           (and runuser (list "runuser" "-u" owner "--")) cmd-and-args)))

(defun lxc-ls (&optional owner &rest args)
  "Return the lines of output from lxc-ls(1) called with ARGS and for OWNER."
  (lines (apply #'lxc-cmd owner "lxc-ls" "-1" args)))

(defun user-container-active-p (host &optional owner)
  (and (not (service:no-services-p))
       (memstr= (get-hostname host) (lxc-ls owner "--active"))))

(defun user-container-running-p (host &optional owner)
  (and (not (service:no-services-p))
       (memstr= (get-hostname host) (lxc-ls owner "--running"))))
