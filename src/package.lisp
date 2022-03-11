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
                          #:unix-namestring
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
           #:unix-namestring
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

           ;; libc.lisp
           #:uid_t
           #:gid_t

           #:+CLONE_NEWCGROUP+
           #:+CLONE_NEWIPC+
           #:+CLONE_NEWNET+
           #:+CLONE_NEWNS+
           #:+CLONE_NEWPID+
           #:+CLONE_NEWTIME+
           #:+CLONE_NEWUSER+
           #:+CLONE_NEWUTS+

           #:+NS_GET_OWNER_UID+

           ;; util.lisp
           #:multiple-value-mapcan
           #:lines
           #:unlines
           #:words
           #:unwords
           #:memstr=
           #:define-simple-error
           #:plist->long-options
           #:systemd--user
	   #:with-local-temporary-directory
           #:pathname-file
           #:local-directory-contents
           #:ensure-trailing-slash
           #:drop-trailing-slash
           #:define-print-object-for-structlike
           #:chroot-pathname
           #:in-chroot-pathname
           #:sh-escape
           #:defpackage-consfig
           #:lambda-ignoring-args
           #:parse-cidr
           #:random-alphanumeric

           #:*consfigurator-debug-level*
           #:with-indented-inform
           #:inform
           #:informat

           #:version<
           #:version>
           #:version<=
           #:version>=

           #:string->filename
           #:filename->string

           #:return-exit
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
           #:connection-readfile
           #:connection-readfile-and-remove
           #:connection-writefile
           #:connection-teardown
           #:connection-connattr
           #:propagate-connattr

           #:run
           #:mrun
           #:with-remote-temporary-file
           #:mkstemp-cmd
           #:mktemp
           #:with-remote-current-directory
           #:run-failed
           #:failed-cmd
           #:failed-stdout
           #:failed-stderr
           #:failed-exit-code
           #:runlines
           #:remote-test
           #:remote-exists-p
           #:remote-file-stats
           #:remote-last-reboot
           #:remote-executable-find
           #:remote-mount-point-p
           #:delete-remote-trees
           #:empty-remote-directory
           #:readfile
           #:writefile
           #:get-connattr
           #:with-connattrs

           ;; property.lisp
           #:collapse-types
           #:propapptype
           #:propappargs
           #:propappdesc
           #:propappattrs
           #:propappcheck
           #:propappapply
           #:propappunapply
           #:ignoring-hostattrs
           #:defprop
           #:defpropspec
           #:defproplist
           #:inapplicable-property
           #:get-hostattrs
           #:get-hostattrs-car
           #:get-parent-hostattrs
           #:get-parent-hostattrs-car
           #:push-hostattrs
           #:pushnew-hostattr
           #:pushnew-hostattrs
           #:get-hostname
           #:get-short-hostname
           #:require-data
           #:failed-change
           #:aborted-change
           #:assert-euid-root
           #:maybe-writefile-string
           #:with-change-if-changes-file
           #:with-change-if-changes-files
           #:with-change-if-changes-file-content

           #:ptype
           #:plambda
           #:papply
           #:punapply

           ;; propspec.lisp
           #:in-consfig
           #:no-consfig
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
           #:localhd
           #:continue-deploy*
           #:evals

           ;; data.lisp
           #:data
           #:iden1
           #:iden2
           #:data-version
           #:data-mime
           #:string-data
           #:data-string
           #:file-data
           #:data-file
	   #:data-source-providing-p
           #:maybe-writefile-data
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
           #:skip-data-source
           #:get-data-stream
           #:with-data-stream
           #:get-data-string
           #:connection-upload
           #:connection-clear-data-cache
           #:upload-all-prerequisite-data
           #:passphrase
           #:make-passphrase
           #:get-data-protected-string

           ;; image.lisp
           #:eval-in-grandchild
           #:eval-in-reinvoked
           #:dump-consfigurator-in-grandchild
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
                    #:+ACL-USER+
                    #:+ACL-GROUP+
                    #:+ACL-TYPE-ACCESS+
                    #:+ACL-TYPE-DEFAULT+
                    #:+ACL-NEXT-ENTRY+
                    #:+ACL-FIRST-ENTRY+

                    #:with-acl-free
                    #:acl-get-file
                    #:acl-set-file
                    #:acl-get-entry
                    #:acl-get-tag-type
                    #:acl-get-qualifier
                    #:acl-set-qualifier

                    #:+CAP-CHOWN+
                    #:+CAP-DAC-OVERRIDE+
                    #:+CAP-DAC-READ-SEARCH+
                    #:+CAP-FOWNER+
                    #:+CAP-FSETID+
                    #:+CAP-KILL+
                    #:+CAP-SETGID+
                    #:+CAP-SETUID+

                    #:+CAP-SETPCAP+
                    #:+CAP-LINUX-IMMUTABLE+
                    #:+CAP-NET-BIND-SERVICE+
                    #:+CAP-NET-BROADCAST+
                    #:+CAP-NET-ADMIN+
                    #:+CAP-NET-RAW+
                    #:+CAP-IPC-LOCK+
                    #:+CAP-IPC-OWNER+
                    #:+CAP-SYS-MODULE+
                    #:+CAP-SYS-RAWIO+
                    #:+CAP-SYS-CHROOT+
                    #:+CAP-SYS-PTRACE+
                    #:+CAP-SYS-PACCT+
                    #:+CAP-SYS-ADMIN+
                    #:+CAP-SYS-BOOT+
                    #:+CAP-SYS-NICE+
                    #:+CAP-SYS-RESOURCE+
                    #:+CAP-SYS-TIME+
                    #:+CAP-SYS-TTY-CONFIG+
                    #:+CAP-MKNOD+
                    #:+CAP-LEASE+
                    #:+CAP-AUDIT-WRITE+
                    #:+CAP-AUDIT-CONTROL+
                    #:+CAP-SETFCAP+
                    #:+CAP-MAC-OVERRIDE+
                    #:+CAP-MAC-ADMIN+
                    #:+CAP-SYSLOG+
                    #:+CAP-WAKE-ALARM+
                    #:+CAP-BLOCK-SUSPEND+
                    #:+CAP-AUDIT-READ+
                    #:+CAP-PERFMON+
                    #:+CAP-BPF+
                    #:+CAP-CHECKPOINT-RESTORE+

                    #:posix-capability-p))

  (package :consfigurator.util.linux-namespace
           (:use #:consfigurator.util.posix1e #:cffi)
           (:export #:get-ids-offset
                    #:reduce-id-maps
                    #:shift-ids
                    #:setgroups-p
                    #:get-userns-owner))

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
                    #:data-uploaded
                    #:host-data-uploaded
                    #:secret-uploaded
                    #:host-secret-uploaded
                    #:data-cache-purged
                    #:contains-conf-equals
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
           (:shadow #:set)
           (:export #:set))

  (package :consfigurator.property.os
           (:shadow #:typecase #:etypecase)
           (:export #:unixlike
                    #:linux
                    #:linux-architecture
                    #:debianlike
                    #:debian
                    #:debian-stable
                    #:debian-testing
                    #:debian-unstable
                    #:debian-experimental
                    #:debian-suite
                    #:debian-architecture
                    #:typecase
                    #:host-typecase
                    #:etypecase
                    #:host-etypecase
                    #:required
                    #:supports-arch-p))

  (package :consfigurator.property.container
           (:export #:contained
                    #:when-contained))

  (package :consfigurator.property.periodic
           (:local-nicknames (#:file  #:consfigurator.property.file))
           (:export #:at-most))

  (package :consfigurator.property.mount
           (:local-nicknames (#:os    #:consfigurator.property.os)
                             (#:cmd   #:consfigurator.property.cmd)
                             (#:file  #:consfigurator.property.file))
           (:export #:mounted
                    #:unmounted-below
                    #:unmounted-below-and-removed
                    #:all-mounts
                    #:*standard-linux-vfs*
                    #:*linux-efivars-vfs*
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
           (:export #:installed
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
                    #:mirror
                    #:uses-parent-mirrors
                    #:proxy
                    #:uses-parent-proxy
                    #:uses-local-cacher
                    #:get-mirrors
                    #:standard-sources.list
                    #:additional-sources
                    #:cache-cleaned
                    #:trusts-key
                    #:all-installed-p
                    #:none-installed-p
                    #:suites-available-pinned
                    #:pinned
                    #:no-pdiffs))

  (package :consfigurator.property.package
           (:local-nicknames (#:apt #:consfigurator.property.apt))
           (:export #:*consfigurator-system-dependencies*
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
                    #:has-groups
                    #:has-desktop-groups
	            #:has-login-shell
                    #:has-enabled-password
                    #:has-locked-password
	            #:passwd-entry))

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
                    #:volume-bootloader
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

                    #:with-these-open-volumes

                    #:has-volumes
                    #:raw-image-built-for
                    #:debian-live-iso-built
                    #:debian-live-iso-built.
                    #:host-volumes-created
                    #:host-logical-volumes-exist

                    #:volumes))

  (package :consfigurator.property.fstab
           (:use #:consfigurator.property.disk)
           (:local-nicknames (#:os    #:consfigurator.property.os)
                             (#:file  #:consfigurator.property.file))
           (:export #:volume->entry
                    #:entries
                    #:entries-for-volumes
                    #:entries-for-opened-volumes))

  (package :consfigurator.property.crypttab
           (:use #:consfigurator.property.disk)
           (:local-nicknames (#:re    #:cl-ppcre)
                             (#:os    #:consfigurator.property.os)
                             (#:file  #:consfigurator.property.file))
           (:export #:volume->entry
                    #:entries-for-opened-volumes))

  (package :consfigurator.property.gnupg
           (:local-nicknames (#:re        #:cl-ppcre))
           (:export #:public-key-imported
                    #:secret-key-imported))

  (package :consfigurator.property.git
           (:local-nicknames (#:os        #:consfigurator.property.os)
                             (#:file      #:consfigurator.property.file)
                             (#:apt       #:consfigurator.property.apt))
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
                    #:get-host-public-keys
                    #:has-host-public-key
                    #:has-host-key))

  (package :consfigurator.property.ssh
           (:local-nicknames (#:file      #:consfigurator.property.file)
                             (#:sshd      #:consfigurator.property.sshd))
           (:export #:authorized-keys
                    #:has-user-key
                    #:known-host
                    #:globally-known-host
                    #:parent-is-globally-known-host))

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
           (:export #:rebooted-at-end))

  (package :consfigurator.property.installer
           (:use #:consfigurator.property.disk #:cffi)
           (:local-nicknames (#:os        #:consfigurator.property.os)
                             (#:cmd       #:consfigurator.property.cmd)
                             (#:file      #:consfigurator.property.file)
                             (#:chroot    #:consfigurator.property.chroot)
                             (#:mount     #:consfigurator.property.mount)
                             (#:fstab     #:consfigurator.property.fstab)
                             (#:reboot    #:consfigurator.property.reboot)
                             (#:crypttab  #:consfigurator.property.crypttab))
           (:export #:install-bootloader-propspec
                    #:install-bootloader-binaries-propspec
                    #:chroot-installed-to-volumes
                    #:bootloader-binaries-installed
                    #:bootloaders-installed
                    #:cleanly-installed-once))

  (package :consfigurator.property.grub
           (:use #:consfigurator.property.disk
                 #:consfigurator.property.installer)
           (:local-nicknames (#:os        #:consfigurator.property.os)
                             (#:file      #:consfigurator.property.file)
                             (#:apt       #:consfigurator.property.apt))
           (:export #:grub
                    #:grub-installed))

  (package :consfigurator.property.u-boot
           (:use #:consfigurator.property.disk
                 #:consfigurator.property.installer)
           (:local-nicknames (#:os        #:consfigurator.property.os)
                             (#:apt       #:consfigurator.property.apt))
           (:export #:u-boot-install-rockchip
                    #:u-boot-installed-rockchip))

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
                             (#:apt       #:consfigurator.property.apt))
           (:export #:installed
                    #:default-network-started
                    #:default-network-autostarted
                    #:defined
                    #:started
                    #:destroyed
                    #:when-started
                    #:kvm-boots-chroot-for
                    #:kvm-boots-chroot-for.
                    #:kvm-boots-chroot
                    #:kvm-boots-chroot.
                    #:virsh-get-columns))

  (package :consfigurator.property.ccache
           (:local-nicknames (#:os        #:consfigurator.property.os)
                             (#:file      #:consfigurator.property.file)
                             (#:apt       #:consfigurator.property.apt))
           (:export #:installed
                    #:has-limits
                    #:group-cache))

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
                    #:user-crontab))

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
                    #:service
                    #:policy
                    #:zone
                    #:has-zone
                    #:zone-target
                    #:default-route-zoned-once
                    #:zone-has-interface
                    #:zone-has-source
                    #:zone-has-service
                    #:zone-masquerade
                    #:zone-rich-rule
                    #:direct-rule
                    #:default-zone))

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
                    #:when-user-container-running
                    #:user-containers-autostart
                    #:usernet-usable-by
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
                    #:has-owner
                    #:has-group
                    #:can-login))

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
                    #:datadir))

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

  (package :consfigurator.data.asdf)

  (package :consfigurator.data.pgp
           (:export #:list-data #:get-data #:set-data #:set-data-from-file))

  (package :consfigurator.data.git-snapshot)

  (package :consfigurator.data.gpgpubkeys)

  (package :consfigurator.data.ssh-askpass
           (:local-nicknames (#:re   #:cl-ppcre)))

  (package :consfigurator.data.local-file)

  (package :consfigurator.data.files-tree))
