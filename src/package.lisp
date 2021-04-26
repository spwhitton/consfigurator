(in-package :cl-user)

(defpackage :consfigurator
  (:use #:cl #:alexandria)
  (:local-nicknames (#:re #:cl-ppcre))
  (:shadowing-import-from #:uiop
                          #:strcat
                          #:string-prefix-p
                          #:split-string
                          #:last-char
                          #:escape-sh-command
                          #:escape-sh-token
                          #:run-program
                          #:read-file-string
                          #:copy-stream-to-stream
                          #:subprocess-error
                          #:stripln
                          #:unix-namestring
                          #:pathname-directory-pathname
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
			  #:delete-directory-tree)
  (:export ;; re-export from UIOP
           #:strcat
           #:string-prefix-p
           #:split-string
           #:last-char
           #:escape-sh-command
           #:escape-sh-token
           #:run-program
           #:read-file-string
           #:copy-stream-to-stream
           #:subprocess-error
           #:stripln
           #:unix-namestring
           #:pathname-directory-pathname
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

           ;; util.lisp
           #:lines
           #:unlines
           #:noop
           #:symbol-named
           #:memstring=
           #:plist-to-cmd-args
	   #:with-local-temporary-directory
           #:pathname-file
           #:quote-nonselfeval
           #:define-print-object-for-structlike
           #:chroot-pathname

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

           ;; connection.lisp
           #:establish-connection
           #:continue-connection
           #:preprocess-connection-args
           #:connection
           #:lisp-connection
           #:posix-connection
           #:connection-parent
           #:lisp-connection-p
           #:reset-remote-home
           #:connection-run
           #:connection-readfile
           #:connection-writefile
           #:connection-teardown

           #:run
           #:mrun
           #:with-remote-temporary-file
           #:with-remote-current-directory
           #:run-failed
           #:runlines
           #:test
           #:remote-exists-p
           #:delete-remote-trees
           #:readfile
           #:writefile
           #:with-local-connection
           #:with-local-passwordless-sudo-connection

           ;; property.lisp
           #:propattrs
           #:propunapply
           #:collapse-types
           #:propapptype
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
           #:require-data
           #:failed-change
           #:assert-euid-root
           #:get-user
           #:assert-connection-supports
           #:call-with-os
           #:with-change-if-changes-file
           #:with-change-if-changes-file-content
           #:with-change-if-changes-file-content-or-mode

           ;; propspec.lisp
           #:in-consfig
           #:propspec-systems
           #:propspec-props
           #:make-propspec
           #:append-propspecs

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
           #:with-preserve-hostattrs
           #:with-replace-hostattrs

           ;; deployment.lisp
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
           #:continue-deploy*

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
           #:request-lisp-systems
           #:passphrase
           #:make-passphrase
           #:get-data-protected-string
           #:continue-deploy*-program))

(defpackage :consfigurator.property.cmd
  (:use #:cl #:consfigurator)
  (:export #:single))

(defpackage :consfigurator.property.file
  (:use #:cl #:consfigurator #:alexandria)
  (:local-nicknames (#:re #:cl-ppcre))
  (:export #:map-file-lines
           #:has-content
           #:contains-lines
           #:lacks-lines
           #:has-mode
           #:does-not-exist
           #:data-uploaded
           #:host-data-uploaded
           #:secret-uploaded
           #:host-secret-uploaded
           #:contains-conf-equals
           #:contains-conf-space
           #:contains-conf-shell
           #:contains-ini-settings
           #:regex-replaced-lines
           #:directory-exists
           #:symlinked
           #:is-copy-of))

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
           #:debian-suite
           #:debian-architecture
           #:typecase
           #:host-typecase
           #:etypecase
           #:host-etypecase
           #:required
           #:supports-arch-p))

(defpackage :consfigurator.property.mount
  (:use #:cl #:alexandria #:consfigurator)
  (:local-nicknames (#:os    #:consfigurator.property.os)
                    (#:cmd   #:consfigurator.property.cmd))
  (:export #:unmounted-below
           #:unmounted-below-and-removed))

(defpackage :consfigurator.property.service
  (:use #:cl #:alexandria #:consfigurator)
  (:local-nicknames (#:os    #:consfigurator.property.os)
                    (#:file  #:consfigurator.property.file))
  (:export #:no-services
           #:running
           #:without-starting-services))

(defpackage :consfigurator.property.apt
  (:use #:cl #:alexandria #:consfigurator)
  (:local-nicknames (#:re         #:cl-ppcre)
                    (#:cmd        #:consfigurator.property.cmd)
                    (#:file       #:consfigurator.property.file)
                    (#:os         #:consfigurator.property.os)
                    (#:service    #:consfigurator.property.service))
  (:export #:installed
           #:removed
           #:reconfigured
           #:service-installed-running
           #:all-configured
           #:updated
           #:periodic-updates
           #:unattended-upgrades
           #:mirror
           #:uses-parent-mirror
           #:proxy
           #:uses-parent-proxy
           #:uses-local-cacher
           #:standard-sources.list
           #:cache-cleaned
           #:all-installed-p
           #:none-installed-p))

(defpackage :consfigurator.connection.sbcl
  (:use #:cl #:alexandria #:consfigurator)
  (:local-nicknames (#:os  #:consfigurator.property.os)
                    (#:apt #:consfigurator.property.apt)))

(defpackage :consfigurator.property.user
  (:use #:cl #:consfigurator)
  (:local-nicknames (#:file  #:consfigurator.property.file))
  (:export #:has-account
	   #:has-login-shell
	   #:passwd-entry))

(defpackage :consfigurator.property.chroot
  (:use #:cl #:consfigurator #:alexandria)
  (:local-nicknames (#:service   #:consfigurator.property.service)
                    (#:apt       #:consfigurator.property.apt)
                    (#:os        #:consfigurator.property.os)
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
           #:subvolumes-of-type

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
           #:lvm-physical-volume

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
           #:linux-swap

           #:has-volumes
           #:caches-cleaned
           #:raw-image-built-for

           #:volumes))

(defpackage :consfigurator.property.fstab
  (:use #:cl #:alexandria #:consfigurator #:consfigurator.property.disk)
  (:local-nicknames (#:os    #:consfigurator.property.os)
                    (#:file  #:consfigurator.property.file))
  (:export #:volume->entry
           #:entry-for-mountpoint
           #:entries-for-volumes))

(defpackage :consfigurator.property.gnupg
  (:use #:cl #:consfigurator)
  (:export #:public-key-imported))

(defpackage :consfigurator.property.git
  (:use #:cl #:consfigurator)
  (:local-nicknames (#:file      #:consfigurator.property.file))
  (:export #:snapshot-extracted))

(defpackage :consfigurator.property.ssh
  (:use #:cl #:consfigurator)
  (:local-nicknames (#:file      #:consfigurator.property.file))
  (:export #:authorized-keys))

(defpackage :consfigurator.property.sshd
  (:use #:cl #:consfigurator)
  (:local-nicknames (#:re        #:cl-ppcre)
                    (#:os        #:consfigurator.property.os)
                    (#:file      #:consfigurator.property.file)
                    (#:apt       #:consfigurator.property.apt))
  (:export #:installed
           #:configured
           #:no-passwords))

(defpackage :consfigurator.property.locale
  (:use #:cl #:consfigurator)
  (:local-nicknames (#:re        #:cl-ppcre)
                    (#:os        #:consfigurator.property.os)
                    (#:apt       #:consfigurator.property.apt)
                    (#:cmd       #:consfigurator.property.cmd)
                    (#:file      #:consfigurator.property.file))
  (:export #:available
           #:selected-for))

(defpackage :consfigurator.connection.shell-wrap
  (:use #:cl #:consfigurator)
  (:export #:shell-wrap-connection #:connection-shell-wrap))

(defpackage :consfigurator.connection.fork
  (:use #:cl #:consfigurator)
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

(defpackage :consfigurator.connection.local
  (:use #:cl #:consfigurator #:alexandria)
  (:export #:local-connection))

(defpackage :consfigurator.connection.chroot
  (:use #:cl
	#:consfigurator
	#:consfigurator.connection.fork
	#:cffi))

(defpackage :consfigurator.connection.chroot.fork
  (:use #:cl
	#:consfigurator
	#:consfigurator.connection.fork
        #:consfigurator.connection.rehome
	#:cffi))

(defpackage :consfigurator.connection.chroot.shell
  (:use #:cl
        #:consfigurator
        #:consfigurator.connection.shell-wrap))

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
