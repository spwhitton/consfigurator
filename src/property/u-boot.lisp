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

(in-package :consfigurator.property.u-boot)
(named-readtables:in-readtable :consfigurator)

(defmethod install-bootloader ((type (eql 'u-boot-install-rockchip))
                               (volume opened-volume)
                               running-on-target &key)
  (mrun "u-boot-install-rockchip" (device-file volume))
  (mrun "sync"))

(defmethod install-bootloader-binaries
    ((type (eql 'u-boot-install-rockchip)) volume &key)
  `(os:etypecase
       (debianlike
        (apt:installed "u-boot-rockchip"))))
