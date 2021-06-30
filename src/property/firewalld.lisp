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

(in-package :consfigurator.property.firewalld)
(named-readtables:in-readtable :consfigurator)

(defproplist installed :posix ()
  (:desc "firewalld installed")
  (os:etypecase
    (debianlike (apt:installed "firewalld"))))

(defprop %firewall-cmd :posix (file warning &rest args)
  (:apply
   ;; --add-service will always tell us ALREADY_ENABLED if nothing was
   ;; changed, but --set-target won't tell us whether a change was made, so we
   ;; have to be prepared to look at whether the file changed, too.
   ;;
   ;; If we make no change to a builtin zone, or similar, then the
   ;; corresponding .xml file may not exist either before or after running the
   ;; command, and given how WITH-CHANGE-IF-CHANGES-FILE works, that means we
   ;; fail to return :NO-CHANGE.  However, we have enough :CHECK subroutines
   ;; defined to avoid this situation actually arising.
   (flet ((run ()
            (let ((output (mrun "firewall-cmd" args)))
              (and warning (search warning output) :no-change))))
     (let ((result (if file
                       (with-change-if-changes-file
                           ((merge-pathnames file #P"/etc/firewalld/")) (run))
                       (run))))
       (unless (eql result :no-change)
         (mrun "firewall-cmd" "--reload"))
       result))))


;;;; Setting contents of XML configuration files

(defproplist %setxml :posix (type name xml)
  (installed)
  (on-change
      (file:exists-with-content #?"/etc/firewalld/${type}/${name}.xml" xml)
    (cmd:single "firewall-cmd" "--reload")))

(defproplist service :posix (service xml)
  (:desc #?"firewalld knows service ${service}")
  (%setxml "services" service xml))

(defproplist policy :posix (policy xml)
  (:desc #?"firewalld has policy ${policy}")
  (%setxml "policies" policy xml))

(defproplist zone :posix (zone xml)
  "Set the whole XML configuration for zone ZONE.

In preference to using this property, it is usually best to incrementally
build up the configuration for a zone using properties like
FIREWALLD:ZONE-HAS-SERVICE, FIREWALLD:ZONE-HAS-INTERFACE etc..  Using this
property forces most of your firewall configuration to be in a single place in
your consfig, but it is typically more readable and flexible to have
properties which set up the actual services and interfaces interact with the
firewall configuration themselves, to render the things that those properties
set up appropriately accessible and inaccessible.

(By contrast, for defining services and policies we take the simpler approach
of just setting the whole XML configuration, using FIREWALLD:SERVICE and
FIREWALLD:POLICY.)"
  (:desc #?"firewalld has zone configuration for ${zone}")
  (%setxml "zones" zone xml))


;;;; Incremental configuration of zones

(defprop has-zone :posix (zone)
  "Ensure that the zone ZONE exists.

You will not usually need to call this property directly; it is applied by
properties which add services, interfaces etc. to zones."
  (:desc #?"firewalld zone ${zone} exists")
  (:check (zerop (mrun :for-exit "firewall-cmd" "--permanent"
                       #?"--zone=${zone}" "--get-target")))
  (:apply (mrun "firewall-cmd" "--permanent" #?"--new-zone=${zone}"))
  (:unapply (mrun "firewall-cmd" "--permanent" #?"--delete-zone=${zone}")))

(defproplist zone-target :posix (zone target)
  (:desc #?"firewalld zone ${zone} has target ${target}")
  (:check (string= target
                   (stripln (run :may-fail "firewall-cmd" "--permanent"
                                 #?"--zone=${zone}" "--get-target"))))
  (installed)
  (has-zone zone)
  (%firewall-cmd #?"zones/${zone}.xml" nil "--permanent"
                 #?"--zone=${zone}" #?"--set-target=${target}"))

(defprop %default-route-zoned :posix (zone)
  (:apply
   (if-let ((default-route-interface
             (loop for line in (runlines "ip" "route" "list" "scope" "global")
                   when (string-prefix-p "default " line)
                     return (fifth (words line)))))
     (%firewall-cmd #?"zones/${zone}.xml" nil
                    "--permanent" #?"--zone=${zone}"
                    #?"--change-interface=${default-route-interface}")
     (failed-change "Could not determine the interface of the default route."))))

(defproplist default-route-zoned-once :posix (&optional (zone "public"))
  "Bind the interface of the default route to zone ZONE, only if this property
has not done that yet for at least one (INTERFACE . ZONE) pair.

This property is intended for machines which have firewalld but do not use
Network Manager, as is typical on Debian servers using firewalld.  On such
machines firewalld will fail to add the primary network interface to any zone
when the interface comes up before firewalld does.

This property avoids the situation in which the primary network interface is
not part of any zone by explicitly adding it to ZONE, determining the name of
the interface by examining the current default route.  The property only adds
an interface to a zone once, as the default route might later be changed
temporarily by something like a VPN connection, and in such a case the
firewall should not be reconfigured.

Typically you will apply both this property and FIREWALLD:DEFAULT-ZONE,
passing the same zone name to each.  If you have Network Manager, you need
only FIREWALLD:DEFAULT-ZONE."
  (with-flagfile "/etc/consfigurator/firewalld/default-route-zoned"
    (installed)
    (has-zone zone)
    (%default-route-zoned zone)))

(defproplist zone-has-interface :posix (zone interface)
  (:desc #?"firewalld zone ${zone} has interface ${interface}")
  (:check (zerop (mrun :for-exit "firewall-cmd" "--permanent"
                       #?"--zone=${zone}"
                       #?"--query-interface=${interface}")))
  (with-unapply
    (installed)
    (has-zone zone)
    (%firewall-cmd #?"zones/${zone}.xml" nil
                   "--permanent" #?"--zone=${zone}"
                   #?"--change-interface=${interface}")
    :unapply (%firewall-cmd #?"zones/${zone}.xml" nil
                            "--permanent" #?"--zone=${zone}"
                            #?"--remove-interface=${interface}")))

(defproplist zone-has-service :posix (zone service)
  (:desc #?"firewalld zone ${zone} has service ${service}")
  (:check (zerop (mrun :for-exit "firewall-cmd" "--permanent"
                       #?"--zone=${zone}" #?"--query-service=${service}")))
  (with-unapply
    (installed)
    (has-zone zone)
    (%firewall-cmd #?"zones/${zone}.xml" "ALREADY_ENABLED"
                   "--permanent" #?"--zone=${zone}"
                   #?"--add-service=${service}")
    :unapply (%firewall-cmd #?"zones/${zone}.xml" "NOT_ENABLED"
                            "--permanent" #?"--zone=${zone}"
                            #?"--remove-service=${service}")))

(defproplist zone-masquerade :posix (zone)
  (:desc #?"firewalld zone ${zone} has masquerade")
  (:check (zerop (mrun :for-exit "firewall-cmd" "--permanent"
                       #?"--zone=${zone}" "--query-masquerade")))
  (with-unapply
    (installed)
    (has-zone zone)
    (%firewall-cmd #?"zones/${zone}.xml" "ALREADY_ENABLED"
                   "--permanent"
                   #?"--zone=${zone}" "--add-masquerade")
    :unapply (%firewall-cmd #?"zones/${zone}.xml" "NOT_ENABLED"
                            "--permanent"
                            #?"--zone=${zone}" "--remove-masquerade")))

(defproplist zone-rich-rule :posix (zone rule)
  (:desc #?"firewalld zone ${zone} has rich rule \"${rule}\"")
  (:check (zerop (mrun :for-exit "firewall-cmd"
                       "--permanent" #?"--zone=${zone}"
                       (strcat "--query-rich-rule=" (escape-sh-token rule)))))
  (with-unapply
    (installed)
    (has-zone zone)
    (%firewall-cmd #?"zones/${zone}.xml" "ALREADY_ENABLED"
                   "--permanent" #?"--zone=${zone}"
                   (strcat "--add-rich-rule=" (escape-sh-token rule)))
    :unapply
    (%firewall-cmd #?"zones/${zone}.xml" "NOT_ENABLED"
                   "--permanent" #?"--zone=${zone}"
                   (strcat "--remove-rich-rule=" (escape-sh-token rule)))))

;; Note that direct rules will be deprecated as of firewalld 1.0.0, as
;; policies and rich rules should be able to cover all uses of direct rules.
;;     <https://firewalld.org/2021/06/the-upcoming-1-0-0>
(defpropspec zone-direct-rule :posix (&rest rule-args)
  (:desc #?"firewalld has direct rule \"@{rule-args}\"")
  (:check (zerop (mrun :for-exit "firewall-cmd"
                       "--permanent" "--direct" "--query-rule" rule-args)))
  `(with-unapply
     (installed)
     (%firewall-cmd "direct.xml" "ALREADY_ENABLED"
                    "--permanent" "--direct" "--add-rule" ,@rule-args)
     :unapply
     (%firewall-cmd "direct.xml" "NOT_ENABLED"
                    "--permanent" "--direct" "--remove-rule" ,@rule-args)))


;;;; Daemon configuration

(defproplist default-zone :posix (zone)
  (:desc #?"firewalld default zone is ${zone}")
  (installed)
  (has-zone zone)
  (%firewall-cmd "firewalld.conf" "ZONE_ALREADY_SET"
                 #?"--set-default-zone=${zone}"))
