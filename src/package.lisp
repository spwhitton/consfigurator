(in-package :cl-user)

(defpackage :consfigurator
  (:use #:cl #:alexandria)
  (:local-nicknames (#:re #:cl-ppcre))
  (:shadowing-import-from #:uiop
                          #:strcat
                          #:string-prefix-p
                          #:string-suffix-p
                          #:split-string
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
                          #:with-temporary-file
                          #:ensure-directory-pathname
                          #:ensure-pathname
                          #:enough-pathname
                          #:subpathp
                          #:getenv
                          #:subdirectories
                          #:directory-files
                          #:file-exists-p
			  #:directory-exists-p
                          #:with-current-directory
			  #:delete-directory-tree
                          #:safe-read-from-string
                          #:compile-file*
                          #:compile-file-pathname*)
  (:export ;; re-export from UIOP
           #:strcat
           #:string-prefix-p
           #:string-suffix-p
           #:split-string
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
           #:with-temporary-file
           #:ensure-directory-pathname
           #:ensure-pathname
           #:enough-pathname
           #:subpathp
           #:getenv
           #:subdirectories
           #:directory-files
           #:file-exists-p
	   #:directory-exists-p
           #:with-current-directory
	   #:delete-directory-tree
           #:safe-read-from-string
           #:compile-file*
           #:compile-file-pathname*

           ;; util.lisp
           #:multiple-value-mapcan
           #:lines
           #:unlines
           #:words
           #:unwords
           #:noop
           #:symbol-named
           #:memstring=
           #:plist-to-cmd-args
	   #:with-local-temporary-directory
           #:pathname-file
           #:ensure-trailing-slash
           #:drop-trailing-slash
           #:quote-nonselfeval
           #:define-print-object-for-structlike
           #:chroot-pathname
           #:in-chroot-pathname
           #:escape-sh-token
           #:escape-sh-command
           #:defpackage-consfig
           #:lambda-ignoring-args

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

           #:unwind-protect-in-parent
           #:cancel-unwind-protect-in-parent-cleanup
           #:with-backtrace-and-exit-code-two

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
           #:connection-writefile
           #:connection-teardown
           #:connection-connattr
           #:propagate-connattr

           #:run
           #:mrun
           #:with-remote-temporary-file
           #:with-remote-current-directory
           #:run-failed
           #:runlines
           #:test
           #:remote-exists-p
           #:remote-file-stats
           #:remote-last-reboot
           #:remote-consfigurator-cache-pathname
           #:delete-remote-trees
           #:readfile
           #:writefile
           #:get-connattr
           #:with-connattrs

           ;; property.lisp
           #:propattrs
           #:propunapply
           #:collapse-types
           #:collapse-propapp-types
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
           #:pushnew-hostattrs
           #:get-hostname
           #:get-short-hostname
           #:require-data
           #:failed-change
           #:assert-euid-root
           #:assert-connection-supports
           #:maybe-writefile-string
           #:call-with-os
           #:with-change-if-changes-file
           #:with-change-if-changes-files
           #:with-change-if-changes-file-content
           #:with-change-if-changes-file-content-or-mode

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
           #:with-requirements
           #:silent-seqprops
           #:unapply
           #:desc
           #:on-change
	   #:as
           #:with-flagfile
           #:with-unapply

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
           #:with-replace-hostattrs

           ;; deployment.lisp
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
           #:get-remote-data-cache-dir

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
           #:asdf-requirements-for-host-and-features
           #:request-asdf-requirements
           #:continue-deploy*-program))

(defpackage :consfigurator.property.cmd
  (:use #:cl #:consfigurator)
  (:export #:single))

(defpackage :consfigurator.property.file
  (:use #:cl #:consfigurator #:alexandria)
  (:local-nicknames (#:re #:cl-ppcre))
  (:export #:map-file-lines
           #:has-content
           #:exists-with-content
           #:contains-lines
           #:lacks-lines
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

(defpackage :consfigurator.property.etc-default
  (:use #:cl #:consfigurator)
  (:local-nicknames (#:file  #:consfigurator.property.file))
  (:shadow #:set)
  (:export #:set))

(defpackage :consfigurator.property.os
  (:use #:cl #:consfigurator)
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

(defpackage :consfigurator.property.container
  (:use #:cl #:consfigurator)
  (:export #:contained
           #:when-contained))

(defpackage :consfigurator.property.periodic
  (:use #:cl #:consfigurator)
  (:local-nicknames (#:file  #:consfigurator.property.file))
  (:export #:at-most))

(defpackage :consfigurator.property.mount
  (:use #:cl #:alexandria #:consfigurator)
  (:local-nicknames (#:os    #:consfigurator.property.os)
                    (#:cmd   #:consfigurator.property.cmd)
                    (#:file  #:consfigurator.property.file))
  (:export #:mounted
           #:unmounted-below
           #:unmounted-below-and-removed))

(defpackage :consfigurator.property.service
  (:use #:cl #:alexandria #:consfigurator)
  (:local-nicknames (#:os    #:consfigurator.property.os)
                    (#:file  #:consfigurator.property.file))
  (:export #:no-services
           #:running
           #:restarted
           #:reloaded
           #:without-starting-services))

(defpackage :consfigurator.property.apt
  (:use #:cl #:alexandria #:consfigurator)
  (:local-nicknames (#:re         #:cl-ppcre)
                    (#:cmd        #:consfigurator.property.cmd)
                    (#:file       #:consfigurator.property.file)
                    (#:os         #:consfigurator.property.os)
                    (#:service    #:consfigurator.property.service))
  (:export #:installed
           #:installed-minimally
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
           #:pinned))

(defpackage :consfigurator.connection.sbcl
  (:use #:cl #:alexandria #:consfigurator)
  (:local-nicknames (#:os  #:consfigurator.property.os)
                    (#:apt #:consfigurator.property.apt)))

(defpackage :consfigurator.property.user
  (:use #:cl #:consfigurator)
  (:local-nicknames (#:file  #:consfigurator.property.file)
                    (#:os    #:consfigurator.property.os))
  (:export #:has-account
           #:has-groups
           #:has-desktop-groups
	   #:has-login-shell
           #:has-enabled-password
	   #:passwd-entry))

(defpackage :consfigurator.property.chroot
  (:use #:cl #:consfigurator #:alexandria)
  (:local-nicknames (#:service   #:consfigurator.property.service)
                    (#:apt       #:consfigurator.property.apt)
                    (#:os        #:consfigurator.property.os)
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

(defpackage :consfigurator.property.live-build
  (:use #:cl #:alexandria #:consfigurator)
  (:local-nicknames (#:apt       #:consfigurator.property.apt)
                    (#:os        #:consfigurator.property.os)
                    (#:file      #:consfigurator.property.file)
                    (#:mount     #:consfigurator.property.mount)
                    (#:chroot    #:consfigurator.property.chroot))
  (:export #:installed
           #:image-built
           #:image-built.))

(defpackage :consfigurator.property.disk
  (:use #:cl #:alexandria #:consfigurator)
  (:local-nicknames (#:re      #:cl-ppcre)
                    (#:chroot  #:consfigurator.property.chroot)
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
           #:caches-cleaned
           #:raw-image-built-for
           #:host-volumes-created
           #:host-logical-volumes-exist

           #:parse-volume-size
           #:volumes))

(defpackage :consfigurator.property.fstab
  (:use #:cl #:alexandria #:consfigurator #:consfigurator.property.disk)
  (:local-nicknames (#:os    #:consfigurator.property.os)
                    (#:file  #:consfigurator.property.file))
  (:export #:volume->entry
           #:entries
           #:entries-for-volumes
           #:entries-for-opened-volumes))

(defpackage :consfigurator.property.crypttab
  (:use #:cl #:alexandria #:consfigurator #:consfigurator.property.disk)
  (:local-nicknames (#:re    #:cl-ppcre)
                    (#:os    #:consfigurator.property.os)
                    (#:file  #:consfigurator.property.file))
  (:export #:volume->entry
           #:entries-for-opened-volumes))

(defpackage :consfigurator.property.gnupg
  (:use #:cl #:consfigurator)
  (:local-nicknames (#:re        #:cl-ppcre))
  (:export #:public-key-imported
           #:secret-key-imported))

(defpackage :consfigurator.property.git
  (:use #:cl #:consfigurator)
  (:local-nicknames (#:os        #:consfigurator.property.os)
                    (#:file      #:consfigurator.property.file)
                    (#:apt       #:consfigurator.property.apt))
  (:export #:installed
           #:snapshot-extracted
           #:cloned
           #:pulled))

(defpackage :consfigurator.property.sshd
  (:use #:cl #:consfigurator)
  (:local-nicknames (#:re        #:cl-ppcre)
                    (#:os        #:consfigurator.property.os)
                    (#:file      #:consfigurator.property.file)
                    (#:apt       #:consfigurator.property.apt))
  (:export #:installed
           #:configured
           #:no-passwords
           #:get-host-public-keys
           #:has-host-public-key
           #:has-host-key))

(defpackage :consfigurator.property.ssh
  (:use #:cl #:alexandria #:consfigurator)
  (:local-nicknames (#:file      #:consfigurator.property.file)
                    (#:sshd      #:consfigurator.property.sshd))
  (:export #:authorized-keys
           #:known-host
           #:globally-known-host
           #:parent-is-globally-known-host))

(defpackage :consfigurator.property.locale
  (:use #:cl #:consfigurator)
  (:local-nicknames (#:re        #:cl-ppcre)
                    (#:os        #:consfigurator.property.os)
                    (#:apt       #:consfigurator.property.apt)
                    (#:cmd       #:consfigurator.property.cmd)
                    (#:file      #:consfigurator.property.file))
  (:export #:available
           #:selected-for))

(defpackage :consfigurator.property.installer
  (:use #:cl #:alexandria #:consfigurator #:consfigurator.property.disk)
  (:local-nicknames (#:os        #:consfigurator.property.os)
                    (#:file      #:consfigurator.property.file)
                    (#:chroot    #:consfigurator.property.chroot)
                    (#:fstab     #:consfigurator.property.fstab)
                    (#:crypttab  #:consfigurator.property.crypttab))
  (:export #:install-bootloader-propspec
           #:install-bootloader-binaries-propspec
           #:chroot-installed-to-volumes
           #:bootloader-binaries-installed))

(defpackage :consfigurator.property.grub
  (:use #:cl #:alexandria #:consfigurator
        #:consfigurator.property.disk
        #:consfigurator.property.installer)
  (:local-nicknames (#:os        #:consfigurator.property.os)
                    (#:file      #:consfigurator.property.file)
                    (#:apt       #:consfigurator.property.apt))
  (:export #:grub
           #:grub-installed))

(defpackage :consfigurator.property.u-boot
  (:use #:cl #:alexandria #:consfigurator
        #:consfigurator.property.disk
        #:consfigurator.property.installer)
  (:local-nicknames (#:os        #:consfigurator.property.os)
                    (#:apt       #:consfigurator.property.apt))
  (:export #:u-boot-install-rockchip
           #:u-boot-installed-rockchip))

(defpackage :consfigurator.property.hostname
  (:use #:cl #:consfigurator)
  (:local-nicknames (#:cmd       #:consfigurator.property.cmd)
                    (#:container #:consfigurator.property.container)
                    (#:file      #:consfigurator.property.file))
  (:export #:is
           #:configured
           #:mailname-configured
           #:search-configured))

(defpackage :consfigurator.property.network
  (:use #:cl #:consfigurator)
  (:local-nicknames (#:os        #:consfigurator.property.os)
                    (#:file      #:consfigurator.property.file))
  (:export #:static))

(defpackage :consfigurator.property.libvirt
  (:use #:cl #:alexandria #:consfigurator)
  (:local-nicknames (#:os        #:consfigurator.property.os)
                    (#:cmd       #:consfigurator.property.cmd)
                    (#:file      #:consfigurator.property.file)
                    (#:chroot    #:consfigurator.property.chroot)
                    (#:apt       #:consfigurator.property.apt))
  (:export #:installed
           #:default-network-started
           #:default-network-autostarted
           #:defined
           #:started
           #:when-started
           #:kvm-boots-chroot-for
           #:kvm-boots-chroot-for.
           #:kvm-boots-chroot
           #:kvm-boots-chroot.))

(defpackage :consfigurator.property.ccache
  (:use #:cl #:consfigurator)
  (:local-nicknames (#:os        #:consfigurator.property.os)
                    (#:file      #:consfigurator.property.file)
                    (#:apt       #:consfigurator.property.apt))
  (:export #:installed
           #:has-limits
           #:group-cache))

(defpackage :consfigurator.property.schroot
  (:use #:cl #:consfigurator)
  (:local-nicknames (#:os        #:consfigurator.property.os)
                    (#:file      #:consfigurator.property.file)
                    (#:apt       #:consfigurator.property.apt))
  (:export #:installed
           #:uses-overlays
           #:overlays-in-tmpfs))

(defpackage :consfigurator.property.sbuild
  (:use #:cl #:alexandria #:consfigurator)
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

(defpackage :consfigurator.property.postfix
  (:use #:cl #:alexandria #:consfigurator)
  (:local-nicknames (#:cmd       #:consfigurator.property.cmd)
                    (#:service   #:consfigurator.property.service)
                    (#:apt       #:consfigurator.property.apt)
                    (#:os        #:consfigurator.property.os)
                    (#:file      #:consfigurator.property.file))
  (:export #:installed
           #:reloaded
           #:main-configured
           #:mapped-file))

(defpackage :consfigurator.property.cron
  (:use #:cl #:consfigurator)
  (:local-nicknames (#:service   #:consfigurator.property.service)
                    (#:apt       #:consfigurator.property.apt)
                    (#:os        #:consfigurator.property.os)
                    (#:file      #:consfigurator.property.file))
  (:export #:system-job
           #:nice-system-job))

(defpackage :consfigurator.property.lets-encrypt
  (:use #:cl #:alexandria #:consfigurator)
  (:local-nicknames (#:apt       #:consfigurator.property.apt)
                    (#:os        #:consfigurator.property.os))
  (:export #:installed
           #:agree-tos
           #:certificate-obtained
           #:fullchain-for
           #:chain-for
           #:certificate-for
           #:privkey-for))

(defpackage :consfigurator.property.apache
  (:use #:cl #:consfigurator)
  (:local-nicknames (#:service   #:consfigurator.property.service)
                    (#:apt       #:consfigurator.property.apt)
                    (#:os        #:consfigurator.property.os)
                    (#:file      #:consfigurator.property.file))
  (:export #:installed
           #:reloaded
           #:mod-enabled
           #:conf-enabled
           #:conf-available))

(defpackage :consfigurator.property.systemd
  (:use #:cl #:consfigurator)
  (:export #:started
           #:stopped
           #:enabled
           #:disabled
           #:masked))

(defpackage :consfigurator.connection.local
  (:use #:cl #:consfigurator #:alexandria)
  (:export #:local-connection))

(defpackage :consfigurator.connection.shell-wrap
  (:use #:cl #:consfigurator)
  (:export #:shell-wrap-connection #:connection-shell-wrap))

(defpackage :consfigurator.connection.fork
  (:use #:cl #:consfigurator #:consfigurator.connection.local)
  (:export #:fork-connection
           #:post-fork
	   #:can-probably-fork))

(defpackage :consfigurator.connection.rehome
  (:use #:cl #:consfigurator #:consfigurator.connection.fork)
  (:export #:rehome-connection
           #:datadir))

(defpackage :consfigurator.connection.as
  (:use #:cl
	#:consfigurator
	#:consfigurator.connection.fork
	#:cffi))

(defpackage :consfigurator.connection.ssh
  (:use #:cl
        #:consfigurator
        #:alexandria
        #:consfigurator.connection.shell-wrap))

(defpackage :consfigurator.connection.sudo
  (:use #:cl
        #:consfigurator
        #:alexandria
        #:consfigurator.connection.shell-wrap))

(defpackage :consfigurator.connection.su
  (:use #:cl
        #:consfigurator
        #:consfigurator.connection.shell-wrap))

(defpackage :consfigurator.connection.chroot
  (:use #:cl
        #:alexandria
	#:consfigurator
	#:consfigurator.connection.fork
        #:consfigurator.connection.rehome
        #:consfigurator.connection.shell-wrap
	#:cffi)
  (:local-nicknames (#:disk      #:consfigurator.property.disk)))

(defpackage :consfigurator.connection.setuid
  (:use #:cl
	#:consfigurator
	#:consfigurator.connection.fork
        #:consfigurator.connection.rehome
	#:cffi)
  (:local-nicknames (#:re   #:cl-ppcre)
		    (#:user #:consfigurator.property.user)))

(defpackage :consfigurator.data.asdf
  (:use #:cl #:consfigurator))

(defpackage :consfigurator.data.pgp
  (:use #:cl #:consfigurator #:alexandria)
  (:export #:list-data #:get-data #:set-data #:set-data-from-file))

(defpackage :consfigurator.data.git-snapshot
  (:use #:cl #:consfigurator #:alexandria))

(defpackage :consfigurator.data.gpgpubkeys
  (:use #:cl #:consfigurator))

(defpackage :consfigurator.data.ssh-askpass
  (:use #:cl #:alexandria #:consfigurator)
  (:local-nicknames (#:re   #:cl-ppcre)))

(defpackage :consfigurator.data.local-file
  (:use #:cl #:consfigurator))
