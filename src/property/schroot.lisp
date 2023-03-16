;;; Consfigurator -- Lisp declarative configuration management system

;;; Copyright (C) 2016, 2021  Sean Whitton <spwhitton@spwhitton.name>

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

(in-package :consfigurator.property.schroot)
(named-readtables:in-readtable :consfigurator)

(defproplist installed :posix ()
  "Ensure that schroot(1) is installed."
  (:desc "schroot(1) installed")
  (os:etypecase
    (debianlike (apt:installed "schroot"))))

(defprop uses-overlays :posix ()
  "Indicate that schroots on a host should use 'union-type=overlay'.

Adding this property does not actually ensure that the line
'union-type=overlay' is present in any schroot config files.  See SBUILD:BUILT
for example usage, via SCHROOT:OVERLAYS-IN-TMPFS."
  (:desc "schroots on host use union-type=overlay")
  (:hostattrs (push-hostattr 'uses-overlays t)))

(defprop overlays-in-tmpfs :posix ()
  "Configure schroot(1) such that all schroots with 'union-type=overlay' in
their configuration will run their overlays in a tmpfs.  Unapplicable, so if
the package you are working on FTBFS when overlays are in tmpfs, you can
toggle this off for a host, and then toggle it back on again later.

Implicitly sets SCHROOT:USES-OVERLAYS.

Shell script from <https://wiki.debian.org/sbuild>."
  (:desc "schroot overlays in tmpfs")
  (:hostattrs (push-hostattr 'uses-overlays t))
  (:apply
   (file:has-content "/etc/schroot/setup.d/04tmpfs" #>>~EOF>>
                     #!/bin/sh

                     set -e

                     . "$SETUP_DATA_DIR/common-data"
                     . "$SETUP_DATA_DIR/common-functions"
                     . "$SETUP_DATA_DIR/common-config"

                     if [ $STAGE = "setup-start" ]; then
                         mount -t tmpfs overlay /var/lib/schroot/union/overlay
                     elif [ $STAGE = "setup-recover" ]; then
                         mount -t tmpfs overlay /var/lib/schroot/union/overlay
                     elif [ $STAGE = "setup-stop" ]; then
                         umount -f /var/lib/schroot/union/overlay
                     fi
                     EOF :mode #o755))
  (:unapply (file:does-not-exist "/etc/schroot/setup.d/04tmpfs")))
