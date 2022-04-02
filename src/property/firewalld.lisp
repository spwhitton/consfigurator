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

;; We employ three strategies for determining whether or not a change was
;; already applied: checking for exit status zero from a firewall-cmd(1) query
;; subcommand like --query-masquerade, looking for certain warning messages in
;; the output, and checking whether certain files under /etc/firewalld change.
;; It would be better if we could just look for the warnings, but
;; firewall-cmd(1) is not consistent about emitting a warning when no change
;; was made -- for example, --set-target always just says "success", but
;; --add-service will say "ALREADY_ENABLED".  We can't completely rely on file
;; content changes either because this is no use when changing the runtime
;; configuration, and if we make no change to a built-in zone, or similar, the
;; corresponding .xml file may not exist either before or after running the
;; command, and given how WITH-CHANGE-IF-CHANGES-FILE works, this means we
;; would fail to return :NO-CHANGE.
;;
;; We incorporate :CHECK into :APPLY here because for most commands we need to
;; check runtime and permanent configuration separately, and two properties is
;; unwieldy.  Previously we updated only the permanent configuration, and then
;; did 'firewall-cmd --reload' when we detected that a change had been made.
;; However, that has the side effect of wiping out configuration which should
;; only ever be part of the runtime configuration, perhaps added by scripts.
;; A disadvantage of the current approach is that it is probably more likely
;; to lead to inconsistent runtime configurations.
;;
;; Both WARNING and APPLY are usually different for unapplication, so we rely
;; on WITH-UNAPPLY together with another application of this property when we
;; want to make a property unapplicable, rather than defining :UNAPPLY here.
(defprop %firewall-cmd :posix
    (runtimep &key file warning check complement-check
              apply (offline-apply apply) (--permanent t)
              &aux (check-fn (if complement-check #'plusp #'zerop)))
  (:apply
   (setq check (ensure-list check) apply (ensure-list apply))
   (labels ((search-warning (output)
              (and warning (search output warning) :no-change))
            (permanent-change ()
              (search-warning
               (if (service:no-services-p)
                   ;; Contrary to its manpage, firewall-offline-cmd(1) often
                   ;; exits nonzero when issuing the ALREADY_ENABLED,
                   ;; NOT_ENABLED and ZONE_ALREADY_SET warnings.
                   (handler-bind
                       ((run-failed
                          (lambda (c)
                            (when (and warning
                                       (search warning (run-failed-stdout c)))
                              (return-from permanent-change :no-change)))))
                     (apply #'mrun "firewall-offline-cmd" offline-apply))
                   (if --permanent
                       (apply #'mrun "firewall-cmd" "--permanent" apply)
                       (apply #'mrun "firewall-cmd" apply))))))
     (let* ((runtime-check
              (or (service:no-services-p)
                  (and runtimep check
                       (funcall
                        check-fn
                        (apply #'mrun :for-exit "firewall-cmd" check)))))
            (permanent-check
              (and check
                   (funcall check-fn
                            (apply #'mrun :for-exit
                                   (if (service:no-services-p)
                                       "firewall-offline-cmd"
                                       '("firewall-cmd" "--permanent"))
                                   check))))
            (runtime (if (or runtime-check (not runtimep))
                         :no-change
                         (search-warning (apply #'mrun "firewall-cmd" apply))))
            (permanent
              (if permanent-check
                  :no-change
                  (if file
                      (with-change-if-changes-file
                          ((merge-pathnames file #P"/etc/firewalld/"))
                        (permanent-change))
                      (permanent-change)))))
       (and (eql :no-change permanent) (eql :no-change runtime) :no-change)))))


;;;; Setting contents of XML configuration files

(defprop %reloaded :posix ()
  (:apply (if (service:no-services-p)
              :no-change
              (mrun "firewall-cmd" "--reload"))))

(defproplist %setxml :posix (type name xml)
  (installed)
  (on-change
      (file:exists-with-content #?"/etc/firewalld/${type}/${name}.xml" xml)
    (%reloaded)))

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
  ;; Another option might be to push all the settings to hostattrs and then at
  ;; :APPLY time, generate the whole .xml / run commands to set all the XML.
  (:desc #?"firewalld has zone configuration for ${zone}")
  (%setxml "zones" zone xml))


;;;; Incremental configuration of zones

(defproplist has-zone :posix (zone)
  "Ensure that the zone ZONE exists.

You will not usually need to call this property directly; it is applied by
properties which add services, interfaces etc. to zones."
  (:desc #?"firewalld zone ${zone} exists")
  (on-change (with-unapply
               (%firewall-cmd nil :check `(,#?"--zone=${zone}" "--get-target")
                                  :apply #?"--new-zone=${zone}")
               :unapply
               (%firewall-cmd nil :complement-check t
                                  :check `(,#?"--zone=${zone}" "--get-target")
                                  :apply #?"--delete-zone=${zone}"))
    (%reloaded)))

(defproplist zone-target :posix (zone target)
  (:desc #?"firewalld zone ${zone} has target ${target}")
  (:check (if (service:no-services-p)
              (string= target
                       (stripln (run :may-fail "firewall-offline-cmd"
                                     #?"--zone=${zone}" "--get-target")))
              (string= target
                       (stripln
                        (run :may-fail "firewall-cmd" "--permanent"
                             #?"--zone=${zone}" "--get-target")))))
  (installed)
  (has-zone zone)
  (on-change (%firewall-cmd
              nil :file #?"zones/${zone}.xml"
              :apply `(,#?"--zone=${zone}" ,#?"--set-target=${target}"))
    (%reloaded)))

(defprop %default-route-zoned :posix (zone)
  (:apply
   (aif (loop for line in (runlines "ip" "route" "list" "scope" "global")
              when (string-prefix-p "default " line)
                return (fifth (words line)))
        (%firewall-cmd
         t :file #?"zones/${zone}.xml"
         :check `(,#?"--zone=${zone}" ,#?"--query-interface=${it}")
         :apply `(,#?"--zone=${zone}" ,#?"--change-interface=${it}"))
        (failed-change
         "Could not determine the interface of the default route."))))

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
  (with-unapply
    (installed)
    (has-zone zone)
    (%firewall-cmd
     t :file #?"zones/${zone}.xml"
     :check `(,#?"--zone=${zone}" ,#?"--query-interface=${interface}")
     :apply `(,#?"--zone=${zone}" ,#?"--change-interface=${interface}"))
    :unapply
    (%firewall-cmd
     t :file #?"zones/${zone}.xml" :complement-check t
     :check `(,#?"--zone=${zone}" ,#?"--query-interface=${interface}")
     :apply `(,#?"--zone=${zone}" ,#?"--remove-interface=${interface}"))))

(defproplist zone-has-source :posix (zone source)
  (:desc #?"firewalld zone ${zone} has source ${source}")
  (with-unapply
    (installed)
    (has-zone zone)
    (%firewall-cmd
     t :file #?"zones/${zone}.xml" :warning "ZONE_ALREADY_SET"
     :check `(,#?"--zone=${zone}" ,#?"--query-source=${source}")
     :apply `(,#?"--zone=${zone}" ,#?"--add-source=${source}"))
    :unapply
    (%firewall-cmd
     t :file #?"zones/${zone}.xml" :warning "UNKNOWN_SOURCE"
     :complement-check t
     :check `(,#?"--zone=${zone}" ,#?"--query-source=${source}")
     :apply `(,#?"--zone=${zone}" ,#?"--remove-source=${source}"))))

(defproplist zone-has-service :posix (zone service)
  (:desc #?"firewalld zone ${zone} has service ${service}")
  (with-unapply
    (installed)
    (has-zone zone)
    (%firewall-cmd
     t :file #?"zones/${zone}.xml"
     :warning "ALREADY_ENABLED"
     :check `(,#?"--zone=${zone}" ,#?"--query-service=${service}")
     :apply `(,#?"--zone=${zone}" ,#?"--add-service=${service}"))
    :unapply
    (%firewall-cmd
     t :file #?"zones/${zone}.xml" :warning "NOT_ENABLED"
     :complement-check t
     :check `(,#?"--zone=${zone}" ,#?"--query-service=${service}")
     :apply `(,#?"--zone=${zone}" ,#?"--remove-service=${service}")
     :offline-apply
     `(,#?"--zone=${zone}" ,#?"--remove-service-from-zone=${service}"))))

(defproplist zone-masquerade :posix (zone)
  (:desc #?"firewalld zone ${zone} has masquerade")
  (with-unapply
    (installed)
    (has-zone zone)
    (%firewall-cmd t :file #?"zones/${zone}.xml" :warning "ALREADY_ENABLED"
                     :check `(,#?"--zone=${zone}" "--query-masquerade")
                     :apply `(,#?"--zone=${zone}" "--add-masquerade"))
    :unapply
    (%firewall-cmd t :file #?"zones/${zone}.xml" :warning "NOT_ENABLED"
                     :complement-check t
                     :check `(,#?"--zone=${zone}" "--query-masquerade")
                     :apply `(,#?"--zone=${zone}" "--remove-masquerade"))))

(defproplist zone-rich-rule :posix (zone rule)
  (:desc #?"firewalld zone ${zone} has rich rule \"${rule}\"")
  (with-unapply
    (installed)
    (has-zone zone)
    (%firewall-cmd
     t :file #?"zones/${zone}.xml" :warning "ALREADY_ENABLED"
     :check `(,#?"--zone=${zone}" ,#?"--query-rich-rule=${rule}")
     :apply `(,#?"--zone=${zone}" ,#?"--add-rich-rule=${rule}"))
    :unapply
    (%firewall-cmd
     t :file #?"zones/${zone}.xml" :warning "NOT_ENABLED"
     :complement-check t
     :check `(,#?"--zone=${zone}" ,#?"--query-rich-rule=${rule}")
     :apply `(,#?"--zone=${zone}" ,#?"--remove-rich-rule=${rule}"))))


;;;; Other daemon configuration

;; Note that direct rules will be deprecated as of firewalld 1.0.0, as
;; policies and rich rules should be able to cover all uses of direct rules.
;;     <https://firewalld.org/2021/06/the-upcoming-1-0-0>
(defpropspec direct-rule :posix (&rest rule-args)
  (:desc #?"firewalld has direct rule \"@{rule-args}\"")
  `(with-unapply
     (installed)
     (%firewall-cmd t :file "direct.xml" :warning "ALREADY_ENABLED"
                      :check ("--direct" "--query-rule" ,@rule-args)
                      :apply ("--direct" "--add-rule" ,@rule-args))
     :unapply
     (%firewall-cmd t :file "direct.xml" :warning "NOT_ENABLED"
                      :complement-check t
                      :check ("--direct" "--query-rule" ,@rule-args)
                      :apply ("--direct" "--remove-rule" ,@rule-args))))

(defproplist default-zone :posix (zone)
  (:desc #?"firewalld default zone is ${zone}")
  (installed)
  (has-zone zone)
  (%firewall-cmd nil
                 :file "firewalld.conf" :warning "ZONE_ALREADY_SET"
                 :--permanent nil :apply #?"--set-default-zone=${zone}"))
