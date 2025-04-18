(in-package :cl-user)

(defpackage :consfigurator
  (:use #:cl #:anaphora #:alexandria #:cffi)
  (:local-nicknames (#:re #:cl-ppcre))
  (:shadowing-import-from #:uiop
                          #:strcat
                          #:string-prefix-p
                          #:string-suffix-p
                          #:split-string
                          #:first-char
                          #:last-char
                          #:run-program
                          #:read-file-string
                          #:copy-stream-to-stream
                          #:slurp-stream-string
                          #:subprocess-error
                          #:stripln
                          #:println
                          #:unix-namestring
                          #:parse-unix-namestring
                          #:pathname-directory-pathname
                          #:pathname-parent-directory-pathname
                          #:resolve-symlinks
                          #:with-temporary-file
                          #:ensure-directory-pathname
                          #:ensure-pathname
                          #:enough-pathname
                          #:pathname-equal
                          #:subpathp
                          #:relative-pathname-p
                          #:absolute-pathname-p
                          #:directory-pathname-p
                          #:getenv
                          #:subdirectories
                          #:directory-files
                          #:file-exists-p
			  #:directory-exists-p
                          #:rename-file-overwriting-target
                          #:with-current-directory
                          #:delete-empty-directory
			  #:delete-directory-tree
                          #:with-safe-io-syntax
                          #:slurp-stream-form
                          #:safe-read-file-form
                          #:safe-read-from-string
                          #:compile-file*
                          #:compile-file-pathname*)
  (:shadowing-import-from #:parse-number #:parse-number)
  (:export ;; re-export from UIOP
           #:strcat
           #:string-prefix-p
           #:string-suffix-p
           #:split-string
           #:first-char
           #:last-char
           #:run-program
           #:read-file-string
           #:copy-stream-to-stream
           #:slurp-stream-string
           #:subprocess-error
           #:stripln
           #:println
           #:unix-namestring
           #:parse-unix-namestring
           #:pathname-directory-pathname
           #:pathname-parent-directory-pathname
           #:resolve-symlinks
           #:with-temporary-file
           #:ensure-directory-pathname
           #:ensure-pathname
           #:enough-pathname
           #:pathname-equal
           #:subpathp
           #:relative-pathname-p
           #:absolute-pathname-p
           #:directory-pathname-p
           #:getenv
           #:subdirectories
           #:directory-files
           #:file-exists-p
	   #:directory-exists-p
           #:rename-file-overwriting-target
           #:with-current-directory
           #:delete-empty-directory
	   #:delete-directory-tree
           #:with-safe-io-syntax
           #:slurp-stream-form
           #:safe-read-file-form
           #:safe-read-from-string
           #:compile-file*
           #:compile-file-pathname*

           ;; re-export from PARSE-NUMBER
           #:parse-number

           ;; libc.lisp
           #:uid_t
           #:gid_t

           #:CLONE_NEWCGROUP
           #:CLONE_NEWIPC
           #:CLONE_NEWNET
           #:CLONE_NEWNS
           #:CLONE_NEWPID
           #:CLONE_NEWTIME
           #:CLONE_NEWUSER
           #:CLONE_NEWUTS

           #:NS_GET_OWNER_UID

           ;; util.lisp
           #:multiple-value-mapcan
           #:lines
           #:unlines
           #:words
           #:unwords
           #:strip-prefix
           #:memstr=
           #:define-simple-error
           #:plist-to-long-options
           #:systemd-user-instance-args
	   #:with-local-temporary-directory
           #:pathname-file
           #:local-directory-contents
           #:ensure-trailing-slash
           #:drop-trailing-slash
           #:define-simple-print-object
           #:chroot-pathname
           #:in-chroot-pathname
           #:sh-escape
           #:defpackage-consfig
           #:lambda-ignoring-args
           #:parse-cidr
           #:random-alphanumeric
           #:valid-hostname-p
           #:prog-changes
           #:add-change
           #:return-changes
           #:sh-script-to-single-line
           #:posix-left-trim
           #:posix-right-trim
           #:posix-trim

           #:*consfigurator-debug-level*
           #:with-indented-inform
           #:inform
           #:informat

           #:version<
           #:version>
           #:version<=
           #:version>=

           #:string-to-filename
           #:filename-to-string

           #:exit-code-to-retval
           #:posix-login-environment

           #:define-error-retval-cfun

           #:chroot
           #:unshare

           #:mapc-open-input-streams
           #:mapc-open-output-streams

           ;; connection.lisp
           #:establish-connection
           #:continue-connection
           #:preprocess-connection-args
           #:connection
           #:lisp-connection
           #:posix-connection
           #:connection-parent
           #:lisp-connection-p
           #:connection-run
           #:connection-read-file
           #:connection-read-and-remove-file
           #:connection-write-file
           #:connection-tear-down
           #:connection-connattr
           #:propagate-connattr

           #:run
           #:mrun
           #:with-remote-temporary-file
           #:mkstemp-cmd
           #:mktemp
           #:with-remote-current-directory
           #:run-failed
           #:run-failed-cmd
           #:run-failed-stdout
           #:run-failed-stderr
           #:run-failed-exit
           #:runlines
           #:remote-test
           #:remote-exists-p
           #:remote-exists-every-p
           #:remote-exists-some-p
           #:remote-file-stats
           #:remote-last-reboot
           #:remote-executable-find
           #:remote-mount-point-p
           #:delete-remote-trees
           #:empty-remote-directory
           #:read-remote-file
           #:write-remote-file
           #:get-connattr
           #:with-connattrs

           ;; property.lisp
           #:combine-propapp-types
           #:propapp-type
           #:propapp-args
           #:propapp-desc
           #:propapp-attrs
           #:check-propapp
           #:apply-propapp
           #:unapply-propapp
           #:ignoring-hostattrs
           #:defprop
           #:defpropspec
           #:defproplist
           #:inapplicable-property
           #:get-hostattrs
           #:get-hostattrs-car
           #:get-parent-hostattrs
           #:get-parent-hostattrs-car
           #:push-hostattr
           #:push-hostattrs
           #:pushnew-hostattr
           #:pushnew-hostattrs
           #:get-hostname
           #:get-short-hostname
           #:require-data
           #:failed-change
           #:aborted-change
           #:assert-remote-euid-root
           #:maybe-write-remote-file-string
           #:with-change-if-changes-file
           #:with-change-if-changes-files
           #:with-change-if-changes-file-content

           #:ptype
           #:plambda
           #:papply
           #:punapply

           ;; propspec.lisp
           #:in-consfig
           #:propspec-systems
           #:propspec-props
           #:make-propspec
           #:append-propspecs
           #:propapp

	   ;; combinator.lisp
           #:define-function-property-combinator
           #:define-choosing-property-combinator
           #:seqprops
           #:eseqprops
           #:eseqprops-until
           #:silent-seqprops
           #:unapply
           #:unapplied
           #:reapplied
           #:desc
           #:on-change
           #:on-apply-change
	   #:as
           #:with-flagfile
           #:with-unapply
           #:with-homedir

           ;; host.lisp
           #:host
           #:unpreprocessed-host
           #:defhost
           #:make-host
           #:make-child-host
           #:union-propspec-into-host
           #:replace-propspec-into-host
           #:hostattrs
           #:host-propspec
           #:preprocess-host
           #:ensure-host
           #:with-preserve-hostattrs
           #:has-hostattrs

           ;; deployment.lisp
           #:at-end
           #:consfigure
           #:defdeploy
           #:defdeploy-these
           #:deploy
           #:deploy*
           #:deploys
           #:deploys.
           #:deploy-these
           #:deploys-these.
           #:deploy-these*
           #:deploys-these
           #:hostdeploy
           #:hostdeploy*
           #:hostdeploy-these
           #:hostdeploy-these*
           #:localsudo
           #:localsudon
           #:localhd
           #:continue-deploy*
           #:evals

           ;; data.lisp
           #:data
           #:data-iden1
           #:data-iden2
           #:data-version
           #:data-mime
           #:string-data
           #:data-string
           #:file-data
           #:data-file
	   #:data-source-providing-p
           #:maybe-write-remote-file-data
           #:missing-data-source
           #:data-pathname
	   #:local-data-pathname
           #:remote-data-pathname
           #:get-remote-cached-prerequisite-data
           #:get-local-cached-prerequisite-data
	   #:get-highest-local-cached-prerequisite-data

           #:try-register-data-source
           #:register-data-source
           #:reset-data-sources
           #:get-data-stream
           #:with-data-stream
           #:get-data-string
           #:connection-upload
           #:connection-clear-data-cache
           #:upload-all-prerequisite-data
           #:wrapped-passphrase
           #:wrap-passphrase
           #:unwrap-passphrase
           #:get-data-protected-string
           #:*data-source-gnupghome*
           #:with-reset-data-sources
           #:missing-data

           ;; image.lisp
           #:eval-in-grandchild
           #:eval-in-reinvoked
           #:wrong-execution-context-for-image-dump
           #:image-dumped
           #:asdf-requirements-for-host-and-features
           #:request-asdf-requirements
           #:continue-deploy*-program))

(macrolet
    ((package (package &body options &aux (use (find :use options :key #'car)))
       `(defpackage ,package
          (:use #:cl #:anaphora #:alexandria #:consfigurator ,@(cdr use))
          ,@(remove use options))))

  (package :consfigurator.util.posix1e
           (:use #:cffi)
           (:export #:acl_type_t
                    #:acl_entry_t
                    #:ACL_USER
                    #:ACL_GROUP
                    #:ACL_TYPE_ACCESS
                    #:ACL_TYPE_DEFAULT
                    #:ACL_NEXT_ENTRY
                    #:ACL_FIRST_ENTRY

                    #:with-acl-free
                    #:acl-get-file
                    #:acl-set-file
                    #:acl-get-entry
                    #:acl-get-tag-type
                    #:acl-get-qualifier
                    #:acl-set-qualifier

                    #:CAP_CHOWN
                    #:CAP_DAC_OVERRIDE
                    #:CAP_DAC_READ_SEARCH
                    #:CAP_FOWNER
                    #:CAP_FSETID
                    #:CAP_KILL
                    #:CAP_SETGID
                    #:CAP_SETUID

                    #:CAP_SETPCAP
                    #:CAP_LINUX_IMMUTABLE
                    #:CAP_NET_BIND_SERVICE
                    #:CAP_NET_BROADCAST
                    #:CAP_NET_ADMIN
                    #:CAP_NET_RAW
                    #:CAP_IPC_LOCK
                    #:CAP_IPC_OWNER
                    #:CAP_SYS_MODULE
                    #:CAP_SYS_RAWIO
                    #:CAP_SYS_CHROOT
                    #:CAP_SYS_PTRACE
                    #:CAP_SYS_PACCT
                    #:CAP_SYS_ADMIN
                    #:CAP_SYS_BOOT
                    #:CAP_SYS_NICE
                    #:CAP_SYS_RESOURCE
                    #:CAP_SYS_TIME
                    #:CAP_SYS_TTY_CONFIG
                    #:CAP_MKNOD
                    #:CAP_LEASE
                    #:CAP_AUDIT_WRITE
                    #:CAP_AUDIT_CONTROL
                    #:CAP_SETFCAP
                    #:CAP_MAC_OVERRIDE
                    #:CAP_MAC_ADMIN
                    #:CAP_SYSLOG
                    #:CAP_WAKE_ALARM
                    #:CAP_BLOCK_SUSPEND
                    #:CAP_AUDIT_READ
                    #:CAP_PERFMON
                    #:CAP_BPF
                    #:CAP_CHECKPOINT_RESTORE

                    #:posix-capability-p))

  (package :consfigurator.property.cmd
           (:export #:single))

  (package :consfigurator.property.file
           (:local-nicknames (#:re #:cl-ppcre))
           (:export #:map-remote-file-lines
                    #:has-content
                    #:exists-with-content
                    #:contains-lines
                    #:lacks-lines
                    #:lacks-lines-matching
                    #:has-mode
                    #:has-ownership
                    #:does-not-exist
                    #:directory-does-not-exist
                    #:empty-directory-does-not-exist
                    #:data-uploaded
                    #:host-data-uploaded
                    #:secret-uploaded
                    #:host-secret-uploaded
                    #:data-cache-purged
                    #:contains-conf-equals
                    #:contains-conf-unspaced
                    #:contains-conf-space
                    #:contains-conf-tab
                    #:contains-conf-shell
                    #:contains-ini-settings
                    #:regex-replaced-lines
                    #:directory-exists
                    #:containing-directory-exists
                    #:symlinked
                    #:is-copy-of
                    #:update-unix-table))

  (package :consfigurator.property.etc-default
           (:local-nicknames (#:file  #:consfigurator.property.file))
           (:export #:contains))

  (package :consfigurator.property.os
           (:shadow #:typecase #:etypecase)
           (:export #:unixlike
                    #:linux
                    #:debianlike
                    #:debian
                    #:debian-stable
                    #:debian-testing
                    #:debian-unstable
                    #:debian-experimental
                    #:debian-suite
                    #:debian-architecture
                    #:debian-architecture-string
                    #:freebsd
                    #:freebsd-release
                    #:freebsd-devel
                    #:freebsd-architecture
                    #:freebsd-version

                    #:typecase
                    #:host-typecase
                    #:etypecase
                    #:host-etypecase

                    #:debian-suite-case
                    #:host-debian-suite-case
                    #:debian-suite-ecase
                    #:host-debian-suite-ecase

                    #:required
                    #:supports-arch-p))

  (package :consfigurator.property.rc.conf
           (:local-nicknames (#:os  #:consfigurator.property.os))
           (:export #:contains
                    #:file-contains
                    #:ws-list-contains
                    #:ws-list-lacks
                    #:file-ws-list-contains
                    #:file-ws-list-lacks))

  (package :consfigurator.property.container
           (:export #:contained
                    #:contained-p
                    #:when-contained))

  (package :consfigurator.property.periodic
           (:local-nicknames (#:file  #:consfigurator.property.file))
           (:export #:at-most
                    #:reapplied-at-most))

  (package :consfigurator.property.mount
           (:local-nicknames (#:os    #:consfigurator.property.os)
                             (#:cmd   #:consfigurator.property.cmd)
                             (#:file  #:consfigurator.property.file))
           (:export #:mounted
                    #:unmounted-below
                    #:unmounted-below-and-removed
                    #:all-mounts
                    #:+linux-basic-vfs+
                    #:+linux-efivars-vfs+
                    #:assert-devtmpfs-udev-/dev))

  (package :consfigurator.property.service
           (:local-nicknames (#:os    #:consfigurator.property.os)
                             (#:file  #:consfigurator.property.file))
           (:export #:no-services
                    #:no-services-p
                    #:running
                    #:restarted
                    #:reloaded
                    #:without-starting-services))

  (package :consfigurator.property.apt
           (:local-nicknames (#:re         #:cl-ppcre)
                             (#:cmd        #:consfigurator.property.cmd)
                             (#:file       #:consfigurator.property.file)
                             (#:os         #:consfigurator.property.os)
                             (#:service    #:consfigurator.property.service))
           (:export #:known-installed-removed-packages-reset
                    #:installed
                    #:installed-minimally
                    #:backports-installed
                    #:backports-installed-minimally
                    #:removed
                    #:reconfigured
                    #:service-installed-running
                    #:all-configured
                    #:updated
                    #:upgraded
                    #:autoremoved
                    #:periodic-updates
                    #:unattended-upgrades
                    #:mirrors
                    #:uses-parent-mirrors
                    #:proxy
                    #:uses-parent-proxy
                    #:uses-local-cacher
                    #:get-mirrors
                    #:standard-sources.list
                    #:additional-suites
                    #:additional-sources
                    #:cache-cleaned
                    #:trusts-key
                    #:all-installed-p
                    #:none-installed-p
                    #:suites-available-pinned
                    #:pinned
                    #:no-pdiffs))

  (package :consfigurator.property.pkgng
           (:local-nicknames (#:os      #:consfigurator.property.os))
           (:export #:installed
                    #:deleted
                    #:upgraded
                    #:autoremoved
                    #:cache-cleaned
                    #:cache-emptied))

  (package :consfigurator.property.package
           (:local-nicknames (#:apt   #:consfigurator.property.apt)
                             (#:pkgng #:consfigurator.property.pkgng))
           (:export #:+consfigurator-system-dependencies+
                    #:package-manager-not-found
                    #:installed))

  (package :consfigurator.connection.sbcl
           (:local-nicknames (#:os      #:consfigurator.property.os)
                             (#:package #:consfigurator.property.package)))

  (package :consfigurator.property.user
           (:local-nicknames (#:file  #:consfigurator.property.file)
                             (#:os    #:consfigurator.property.os))
           (:export #:has-account
                    #:has-account-with-uid
                    #:group-exists
                    #:has-groups
                    #:has-desktop-groups
	            #:has-login-shell
                    #:has-enabled-password
                    #:has-locked-password
	            #:passwd-field
                    #:user-info))

  (package :consfigurator.util.linux-namespace
           (:use #:consfigurator.util.posix1e #:cffi)
           (:local-nicknames (#:user #:consfigurator.property.user))
           (:export #:get-ids-offset
                    #:reduce-id-maps
                    #:shift-ids
                    #:setgroups-p
                    #:get-userns-owner))

  (package :consfigurator.property.chroot
           (:local-nicknames (#:service   #:consfigurator.property.service)
                             (#:apt       #:consfigurator.property.apt)
                             (#:os        #:consfigurator.property.os)
                             (#:package   #:consfigurator.property.package)
                             (#:container #:consfigurator.property.container)
                             (#:mount     #:consfigurator.property.mount)
                             (#:file      #:consfigurator.property.file))
           (:shadow #:deploys #:deploys. #:deploys-these #:deploys-these.)
           (:export #:deploys
                    #:deploys.
                    #:deploys-these
                    #:deploys-these.
                    #:os-bootstrapped-for
                    #:os-bootstrapped-for.
                    #:os-bootstrapped
                    #:os-bootstrapped.))

  (package :consfigurator.property.disk
           (:local-nicknames (#:re      #:cl-ppcre)
                             (#:chroot  #:consfigurator.property.chroot)
                             (#:cmd     #:consfigurator.property.cmd)
                             (#:file    #:consfigurator.property.file)
                             (#:os      #:consfigurator.property.os)
                             (#:apt     #:consfigurator.property.apt))
           (:export #:volume
                    #:volume-label
                    #:volume-contents
                    #:volume-size
                    #:volume-bootloaders
                    #:subvolumes-of-type
                    #:all-subvolumes
                    #:copy-volume-and-contents
                    #:require-volumes-data
                    #:opened-volume
                    #:device-file

                    #:physical-disk
                    #:disk-image
                    #:image-file
                    #:raw-disk-image
                    #:opened-raw-disk-image
                    #:partitioned-volume
                    #:opened-partitioned-volume
                    #:partition
                    #:opened-partition

                    #:lvm-volume-group
                    #:lvm-logical-volume
                    #:activated-lvm-logical-volume
                    #:lvm-physical-volume
                    #:opened-lvm-physical-volume

                    #:filesystem
                    #:mount-point
                    #:mount-options
                    #:mounted-filesystem
                    #:ext4-filesystem
                    #:mounted-ext4-filesystem
                    #:fat32-filesystem
                    #:mounted-fat32-filesystem

                    #:luks-container
                    #:opened-luks-container
                    #:crypttab-options
                    #:crypttab-keyfile
                    #:linux-swap

                    #:opened-volumes
                    #:opened-volume-parents
                    #:with-opened-volumes

                    #:has-volumes
                    #:raw-image-built-for
                    #:first-disk-installed-for
                    #:volumes-installed-for
                    #:debian-live-iso-built
                    #:debian-live-iso-built.
                    #:host-logical-volumes-exist

                    #:volumes))

  (package :consfigurator.property.fstab
           (:use #:consfigurator.property.disk)
           (:local-nicknames (#:os    #:consfigurator.property.os)
                             (#:file  #:consfigurator.property.file)
                             (#:disk  #:consfigurator.property.disk))
           (:export #:volume-to-entry
                    #:has-entries
                    #:has-entries-for-volumes
                    #:has-entries-for-opened-volumes))

  (package :consfigurator.property.crypttab
           (:use #:consfigurator.property.disk)
           (:local-nicknames (#:re    #:cl-ppcre)
                             (#:os    #:consfigurator.property.os)
                             (#:file  #:consfigurator.property.file)
                             (#:disk  #:consfigurator.property.disk))
           (:export #:volume-to-entry
                    #:has-entries-for-opened-volumes))

  (package :consfigurator.property.gnupg
           (:local-nicknames (#:re        #:cl-ppcre))
           (:export #:public-key-imported
                    #:secret-key-imported))

  (package :consfigurator.property.git
           (:local-nicknames (#:os        #:consfigurator.property.os)
                             (#:file      #:consfigurator.property.file)
                             (#:apt       #:consfigurator.property.apt)
                             (#:pkgng     #:consfigurator.property.pkgng))
           (:export #:installed
                    #:snapshot-extracted
                    #:cloned
                    #:pulled
                    #:repo-configured))

  (package :consfigurator.property.sshd
           (:local-nicknames (#:re        #:cl-ppcre)
                             (#:os        #:consfigurator.property.os)
                             (#:file      #:consfigurator.property.file)
                             (#:apt       #:consfigurator.property.apt)
                             (#:service   #:consfigurator.property.service))
           (:export #:installed
                    #:configured
                    #:no-passwords
                    #:host-public-keys
                    #:has-host-public-key
                    #:has-host-key))

  (package :consfigurator.property.ssh
           (:local-nicknames (#:file      #:consfigurator.property.file)
                             (#:sshd      #:consfigurator.property.sshd))
           (:export #:authorized-keys
                    #:has-user-key
                    #:known-host
                    #:system-known-host
                    #:parent-is-system-known-host))

  (package :consfigurator.property.locale
           (:local-nicknames (#:re        #:cl-ppcre)
                             (#:os        #:consfigurator.property.os)
                             (#:apt       #:consfigurator.property.apt)
                             (#:cmd       #:consfigurator.property.cmd)
                             (#:file      #:consfigurator.property.file))
           (:export #:available
                    #:selected-for))

  (package :consfigurator.property.reboot
           (:local-nicknames (#:container #:consfigurator.property.container))
           (:shadow #:at-end)
           (:export #:at-end))

  (package :consfigurator.property.installer
           (:use #:consfigurator.property.disk #:cffi)
           (:local-nicknames (#:os        #:consfigurator.property.os)
                             (#:cmd       #:consfigurator.property.cmd)
                             (#:file      #:consfigurator.property.file)
                             (#:chroot    #:consfigurator.property.chroot)
                             (#:mount     #:consfigurator.property.mount)
                             (#:fstab     #:consfigurator.property.fstab)
                             (#:reboot    #:consfigurator.property.reboot)
                             (#:crypttab  #:consfigurator.property.crypttab)
                             (#:disk      #:consfigurator.property.disk))
           (:export #:install-bootloader-propspec
                    #:install-bootloader-binaries-propspec
                    #:files-installed-to-volumes-for
                    #:bootloader-binaries-installed
                    #:bootloaders-installed
                    #:cleanly-installed-once
                    #:with-cleanly-installed-once))

  (package :consfigurator.property.grub
           (:use #:consfigurator.property.disk
                 #:consfigurator.property.installer)
           (:local-nicknames (#:os        #:consfigurator.property.os)
                             (#:file      #:consfigurator.property.file)
                             (#:apt       #:consfigurator.property.apt)
                             (#:container #:consfigurator.property.container))
           (:export #:grub
                    #:grub-installed))

  (package :consfigurator.property.u-boot
           (:use #:consfigurator.property.disk
                 #:consfigurator.property.installer)
           (:local-nicknames (#:os        #:consfigurator.property.os)
                             (#:apt       #:consfigurator.property.apt)
                             (#:container #:consfigurator.property.container))
           (:export #:install-rockchip
                    #:installed-rockchip))

  (package :consfigurator.property.hostname
           (:local-nicknames (#:cmd       #:consfigurator.property.cmd)
                             (#:container #:consfigurator.property.container)
                             (#:file      #:consfigurator.property.file))
           (:export #:is
                    #:configured
                    #:mailname-configured
                    #:search-configured))

  (package :consfigurator.property.network
           (:local-nicknames (#:os        #:consfigurator.property.os)
                             (#:file      #:consfigurator.property.file))
           (:export #:aliases
                    #:ipv4
                    #:ipv6
                    #:clean-/etc/network/interfaces
                    #:static
                    #:preserve-static-once))

  (package :consfigurator.property.libvirt
           (:local-nicknames (#:os        #:consfigurator.property.os)
                             (#:cmd       #:consfigurator.property.cmd)
                             (#:service   #:consfigurator.property.service)
                             (#:file      #:consfigurator.property.file)
                             (#:chroot    #:consfigurator.property.chroot)
                             (#:apt       #:consfigurator.property.apt)
                             (#:disk      #:consfigurator.property.disk))
           (:export #:installed
                    #:default-network-started
                    #:default-network-autostarted
                    #:defined-for
                    #:started
                    #:destroyed
                    #:when-started
                    #:kvm-boots-chroot-for
                    #:kvm-boots-chroot-for.
                    #:kvm-boots-chroot
                    #:kvm-boots-chroot.
                    #:kvm-boots-lvm-lv-for
                    #:kvm-boots-lvm-lv-for.
                    #:kvm-boots-lvm-lv
                    #:kvm-boots-lvm-lv.
                    #:virsh-get-columns))

  (package :consfigurator.property.ccache
           (:local-nicknames (#:os        #:consfigurator.property.os)
                             (#:file      #:consfigurator.property.file)
                             (#:apt       #:consfigurator.property.apt))
           (:export #:installed
                    #:has-limits
                    #:cache-for-group))

  (package :consfigurator.property.schroot
           (:local-nicknames (#:os        #:consfigurator.property.os)
                             (#:file      #:consfigurator.property.file)
                             (#:apt       #:consfigurator.property.apt))
           (:export #:installed
                    #:uses-overlays
                    #:overlays-in-tmpfs))

  (package :consfigurator.property.sbuild
           (:local-nicknames (#:os        #:consfigurator.property.os)
                             (#:file      #:consfigurator.property.file)
                             (#:chroot    #:consfigurator.property.chroot)
                             (#:user      #:consfigurator.property.user)
                             (#:apt       #:consfigurator.property.apt)
                             (#:ccache    #:consfigurator.property.ccache)
                             (#:schroot   #:consfigurator.property.schroot)
                             (#:periodic  #:consfigurator.property.periodic))
           (:export #:installed
                    #:usable-by
                    #:built
                    #:built.
                    #:standard-debian-schroot))

  (package :consfigurator.property.postfix
           (:local-nicknames (#:cmd       #:consfigurator.property.cmd)
                             (#:service   #:consfigurator.property.service)
                             (#:apt       #:consfigurator.property.apt)
                             (#:os        #:consfigurator.property.os)
                             (#:file      #:consfigurator.property.file)
                             (#:user      #:consfigurator.property.user))
           (:export #:installed
                    #:reloaded
                    #:main-configured
                    #:mapped-file
                    #:daemon-socket-directory))

  (package :consfigurator.property.cron
           (:local-nicknames (#:re        #:cl-ppcre)
                             (#:service   #:consfigurator.property.service)
                             (#:apt       #:consfigurator.property.apt)
                             (#:os        #:consfigurator.property.os)
                             (#:file      #:consfigurator.property.file))
           (:export #:system-job
                    #:nice-system-job
                    #:runs-consfigurator
                    #:user-crontab-installed))

  (package :consfigurator.property.lets-encrypt
           (:local-nicknames (#:apt       #:consfigurator.property.apt)
                             (#:os        #:consfigurator.property.os))
           (:export #:installed
                    #:agree-tos
                    #:certificate-obtained
                    #:certificate-obtained-standalone
                    #:fullchain-for
                    #:chain-for
                    #:certificate-for
                    #:privkey-for))

  (package :consfigurator.property.apache
           (:local-nicknames
            (#:service             #:consfigurator.property.service)
            (#:apt                 #:consfigurator.property.apt)
            (#:os                  #:consfigurator.property.os)
            (#:file                #:consfigurator.property.file)
            (#:network             #:consfigurator.property.network)
            (#:lets-encrypt        #:consfigurator.property.lets-encrypt))
           (:export #:installed
                    #:reloaded
                    #:mod-enabled
                    #:conf-enabled
                    #:conf-available
                    #:site-enabled
                    #:site-available
                    #:https-vhost))

  (package :consfigurator.property.systemd
           (:local-nicknames (#:service #:consfigurator.property.service))
           (:export #:daemon-reloaded
                    #:started
                    #:stopped
                    #:reloaded
                    #:restarted
                    #:enabled
                    #:disabled
                    #:masked
                    #:lingering-enabled))

  (package :consfigurator.property.firewalld
           (:local-nicknames (#:cmd         #:consfigurator.property.cmd)
                             (#:file        #:consfigurator.property.file)
                             (#:apt         #:consfigurator.property.apt)
                             (#:os          #:consfigurator.property.os)
                             (#:service     #:consfigurator.property.service))
           (:export #:installed
                    #:knows-service
                    #:has-policy
                    #:has-zone-xml
                    #:has-zone
                    #:zone-has-target
                    #:default-route-zoned-once
                    #:zone-has-interface
                    #:zone-has-source
                    #:zone-has-service
                    #:zone-has-masquerade
                    #:zone-has-rich-rule
                    #:has-direct-rule
                    #:has-default-zone))

  (package :consfigurator.property.timezone
           (:local-nicknames (#:file         #:consfigurator.property.file)
                             (#:apt          #:consfigurator.property.apt)
                             (#:os           #:consfigurator.property.os))
           (:export #:configured
                    #:configured-from-parent))

  (package :consfigurator.property.swap
           (:local-nicknames (#:cmd          #:consfigurator.property.cmd)
                             (#:fstab        #:consfigurator.property.fstab)
                             (#:os           #:consfigurator.property.os))
           (:export #:has-swap-file))

  (package :consfigurator.property.lxc
           (:use #:consfigurator.util.linux-namespace #:cffi)
           (:local-nicknames (#:file        #:consfigurator.property.file)
                             (#:apt         #:consfigurator.property.apt)
                             (#:os          #:consfigurator.property.os)
                             (#:service     #:consfigurator.property.service)
                             (#:chroot      #:consfigurator.property.chroot)
                             (#:user        #:consfigurator.property.user)
                             (#:systemd     #:consfigurator.property.systemd))
           (:export #:installed
                    #:user-container-started
                    #:user-container-stopped
                    #:when-user-container-running
                    #:user-containers-autostart
                    #:usernet-veth-usable-by
                    #:user-container-for
                    #:user-container-for.
                    #:user-container
                    #:user-container.

                    #:lxc-ls))

  (package :consfigurator.property.postgres
           (:local-nicknames (#:apt         #:consfigurator.property.apt)
                             (#:os          #:consfigurator.property.os)
                             (#:cmd         #:consfigurator.property.cmd))
           (:export #:installed
                    #:superuser-is
                    #:has-role
                    #:has-database
                    #:database-has-owner
                    #:has-group
                    #:user-can-login))

  (package :consfigurator.connection.local
           (:export #:local-connection))

  (package :consfigurator.connection.shell-wrap
           (:export #:shell-wrap-connection #:connection-shell-wrap))

  (package :consfigurator.connection.fork
           (:use #:consfigurator.connection.local)
           (:export #:fork-connection
                    #:post-fork
                    #:init-hooks-connection))

  (package :consfigurator.connection.rehome
           (:use #:consfigurator.connection.fork)
           (:export #:rehome-connection
                    #:rehome-datadir))

  (package :consfigurator.connection.as
           (:use #:consfigurator.connection.fork #:cffi))

  (package :consfigurator.connection.ssh
           (:use #:consfigurator.connection.shell-wrap))

  (package :consfigurator.connection.sudo
           (:use #:consfigurator.connection.shell-wrap))

  (package :consfigurator.connection.su
           (:use #:consfigurator.connection.shell-wrap))

  (package :consfigurator.connection.chroot
           (:use #:consfigurator.connection.fork
                 #:consfigurator.connection.rehome
                 #:consfigurator.connection.shell-wrap
	         #:cffi)
           (:local-nicknames (#:disk      #:consfigurator.property.disk)
                             (#:mount     #:consfigurator.property.mount)))

  (package :consfigurator.connection.setuid
           (:use #:consfigurator.connection.fork
                 #:consfigurator.connection.rehome
	         #:cffi)
           (:local-nicknames (#:re   #:cl-ppcre)
		             (#:user #:consfigurator.property.user)))

  (package :consfigurator.connection.linux-namespace
           (:use #:consfigurator.util.linux-namespace
                 #:consfigurator.connection.fork
                 #:consfigurator.connection.shell-wrap)
           (:local-nicknames (#:user #:consfigurator.property.user)
                             (#:lxc  #:consfigurator.property.lxc)))

  (package :consfigurator.data.util
           (:export #:literal-data-pathname #:gpg-file-as-string #:gpg))

  (package :consfigurator.data.asdf)

  (package :consfigurator.data.pgp
           (:use  #:consfigurator.data.util)
           (:export #:list-data #:get-data #:set-data #:set-data-from-file))

  (package :consfigurator.data.git-snapshot)

  (package :consfigurator.data.gpgpubkeys)

  (package :consfigurator.data.ssh-askpass
           (:local-nicknames (#:re   #:cl-ppcre)))

  (package :consfigurator.data.local-file)

  (package :consfigurator.data.pass
           (:use  #:consfigurator.data.util))

  (package :consfigurator.data.files-tree
           (:use  #:consfigurator.data.util)))
