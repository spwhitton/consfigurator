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
   ;; has some pitfalls.
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
    `(with-unapply
      (apt:service-installed-running "cron")
      (apt:installed "moreutils")
      (file:exists-with-content ,job
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
      (file:exists-with-content ,script
        ,`("#!/bin/sh"
           ""
           "set -e"
           ""
           ;; Use flock(1) to ensure that only one instance of the job is ever
           ;; running, no matter how long one run of the job takes.
           ,(format nil "flock -n ~A sh -c ~A"
                    (escape-sh-token (unix-namestring job))
                    (escape-sh-token shell-command)))
        :mode #o755)
       :unapply (file:does-not-exist ,job ,script))))

(defproplist nice-system-job :posix (desc when user shell-command)
  "Like CRON:SYSTEM-JOB, but run the command niced and ioniced."
  (:desc #?"Cronned ${desc}, niced and ioniced")
  (system-job desc when user (format nil "nice ionice -c 3 sh -c ~A"
                                     (escape-sh-token shell-command))))

(defproplist runs-consfigurator :lisp (when)
  "Re-execute the most recent deployment that included an application of this
property, or of IMAGE-DUMPED with no arguments, using CRON:NICE-SYSTEM-JOB.

This can be useful to ensure that your system remains in a consistent state
between manual deployments, and to ensure the timely application of properties
modified by the PERIODIC:AT-MOST combinator.

For hosts to which this property is applied, mixing usage of DEPLOY and
DEPLOY-THESE (or HOSTDEPLOY and HOSTDEPLOY-THESE, etc.) can lead to some
inconsistent situations.  For example, suppose you

    (hostdeploy foo.example.org (additional-property))

and then later

    (hostdeploy-these foo.example.org (unapplied (additional-property)).

As neither CRON:RUNS-CONFIGURATOR nor IMAGE-DUMPED with no arguments was
applied since ADDITIONAL-PROPERTY was unapplied, the executable invoked by the
CRON:RUNS-CONFIGURATOR cronjob will try to apply ADDITIONAL-PROPERTY again.
One straightforward way to reduce the incidence of this sort of problem would
be to refrain from using the ADDITIONAL-PROPERTIES argument to DEPLOY,
HOSTDEPLOY etc.

You may wish to apply this property within ESEQPROPS-UNTIL; see the docstring
of IMAGE-DUMPED."
  (with-unapply
    (image-dumped)
    (nice-system-job
     "consfigurator" when "root"
     "${XDG_CACHE_HOME:-$HOME/.cache}/consfigurator/images/latest")
    :unapply (unapplied (system-job "consfigurator" when "" ""))))

(defprop user-crontab :posix (env &rest jobs)
  "Set the contents of the current user's crontab.  ENV is like the ENV argument
to RUN/MRUN, except that the environment variables will be set at the top of
the generated crontab.  Each of JOBS is a line for the body of the crontab.
In both ENV and JOBS, the string \"$HOME\" is replaced with the remote home
directory."
  ;; We set the contents of the whole file rather than providing properties to
  ;; specify individual jobs, because then it is straightforward to
  ;; incrementally develop jobs without having to unapply old versions first.
  (:desc "Crontab populated")
  (:apply
   (unless (member :path env)
     (setq env
           (list*
            :path
            (if (zerop (get-connattr :remote-uid))
                "/usr/local/sbin:/usr/local/bin:/sbin:/bin:/usr/sbin:/usr/bin"
                "/usr/local/bin:/bin:/usr/bin")
            env)))
   (let* ((home (drop-trailing-slash
                 (unix-namestring (get-connattr :remote-home))))
          (old (runlines :may-fail "crontab" "-l"))
          (new
            (mapcar
             (lambda (line) (re:regex-replace-all #?/\$HOME/ line home))
             (nconc
              (list "# Automatically updated by Consfigurator; do not edit" "")
              (loop for (k v) on env by #'cddr
                    collect (strcat (string-upcase (symbol-name k)) "=" v))
              (list "")
              jobs))))
     (if (tree-equal old new :test #'string=)
         :no-change
         (mrun :input (unlines new)
               "crontab" "-u" (get-connattr :remote-user) "-")))))
