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

(in-package :consfigurator.property.cron)
(named-readtables:in-readtable :consfigurator)

;;; A number of techniques here are from Propellor's Cron properties module.

(defpropspec system-job :posix (desc when user shell-command)
  "Installs a cronjob running SHELL-COMMAND as USER to /etc/cron.*.
DESC must be unique, as it will be used as a filename for a script.  WHEN is
either :DAILY, WEEKLY, :MONTHLY or a string formatted according to crontab(5),
e.g. \"0 3 * * *\".

The output of the cronjob will be mailed only if the job exits nonzero."
  (:desc #?"Cronned ${desc}")
  (:hostattrs
   ;; /etc/cron.* is Debian-specific.  Also, we rely on runuser(1), which is
   ;; Linux-specific.  This is done because su(1) for non-interactive usage
   ;; has some pitfalls, and the command line argument '-c' is not portable.
   (os:required 'os:debianlike))
  (let* ((times (not (keywordp when)))
         (dir (ensure-directory-pathname
               (strcat "/etc/cron." (if (keywordp when)
                                        (string-downcase (symbol-name when))
                                        "d"))))
         (job (merge-pathnames (string->filename desc) dir))
         (script (merge-pathnames (strcat (string->filename desc) "_cronjob")
                                  #P"/usr/local/bin/"))
         (script* (escape-sh-token (unix-namestring script))))
    `(eseqprops
      (apt:service-installed-running "cron")
      (apt:installed "moreutils")
      (file:has-content ,job
        ,`(,@(and (not times) '("#!/bin/sh" "" "set -e" ""))
           "SHELL=/bin/sh"
           "PATH=/usr/local/sbin:/usr/local/bin:/sbin:/bin:/usr/sbin:/usr/bin"
           ""
           ,@`(,(if times
                    #?"${when}	${user}	chronic ${script*}"
                    (if (string= user "root")
                        (format nil "chronic ~A" script*)
                        (format nil "chronic runuser -u ~A -c ~A"
                                user script*)))))
        ,@(and (not times) '(:mode #o755)))
      ;; Using a separate script makes for more readable e-mail subject lines,
      ;; and also makes it easy to do a manual run of the job.
      (file:has-content ,script
        ,`("#!/bin/sh"
           ""
           "set -e"
           ""
           ;; Use flock(1) to ensure that only one instance of the job is ever
           ;; running, no matter how long one run of the job takes.
           ,(format nil "flock -n ~A sh -c ~A"
                    (escape-sh-token (unix-namestring job))
                    (escape-sh-token shell-command)))
        :mode #o755))))

(defproplist nice-system-job :posix (desc when user shell-command)
  "Like CRON:SYSTEM-JOB, but run the command niced and ioniced."
  (:desc #?"Cronned ${desc}, niced and ioniced")
  (system-job desc when user (format nil "nice ionice -c 3 sh -c ~A"
                                     (escape-sh-token shell-command))))
