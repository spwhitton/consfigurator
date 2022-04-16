;;; Consfigurator -- Lisp declarative configuration management system

;;; Copyright (C) 2021-2022  David Bremner <david@tethera.net>

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

(in-package :consfigurator.property.postgres)
(named-readtables:in-readtable :consfigurator)

(defproplist installed :posix ()
  "Ensure that postgresql and associated utilities are installed."
  (:desc "postgresql and associated utilities installed")
  (os:etypecase
    (debianlike (apt:installed "postgresql" "postgresql-client"))))

(defprop superuser-is :posix (name)
  "Record Postgres superuser"
  (:desc "postgres superuser is ${name}")
  (:hostattrs
   (push-hostattr 'postgres-superuser name)))

(defprop %psql :posix (sql &key unless)
  (:check
   (declare (ignore sql))
   (and
    unless
    (let ((result (string-trim '(#\Space #\Newline #\Tab #\Return)
                               (mrun "psql" "-t" "postgres" :input unless))))
      (informat 4 "~&PSQL=> ~a" result)
      ;; this is case insensitive on purpose.
      (string-equal "yes" result))))
  (:apply
   (declare (ignore unless))
   (mrun :inform "psql" "postgres" :input sql)))

(defproplist %run-sql :posix (sql &key unless)
  (installed)
  (as (or (get-hostattrs-car 'postgres-superuser) "postgres")
    (%psql sql :unless unless)))

(defproplist has-role :posix (role)
  "Ensure ROLE exists in the Postgres cluster."
  (:desc #?"Postgres role ${role} exists")
  (%run-sql
   #?"DO $$
      BEGIN
      CREATE ROLE ${role};
      EXCEPTION WHEN duplicate_object THEN RAISE NOTICE '%, skipping',
        SQLERRM USING ERRCODE = SQLSTATE;
      END
      $$;"
   :unless #?"select 'yes' from pg_roles where rolname='${role}';"))

(defproplist has-database :posix (db-name)
  "Ensure Postgres DATABASE exists"
  (:desc #?"Postgres database ${db-name} exists")
  ;; this has a potential race condition between test and creation
  (%run-sql
   #?"CREATE DATABASE ${db-name};"
   :unless  #?"SELECT 'yes' FROM pg_database WHERE datname = '${db-name}';"))

(defproplist database-has-owner :posix (database owner)
  (:desc #?"Postgres database ${database} has owner ${owner}")
  (%run-sql #?"ALTER DATABASE ${database} OWNER TO ${owner}"
            :unless  #?"select 'yes' from pg_database d, pg_authid a
                        where d.datname='${database}' and d.datdba = a.oid
                        and a.rolname='${owner}';"))

(defproplist has-group :posix (user group)
  "Ensure Postgres user USER is a member of GROUP."
  (:desc #?"Postgres role ${user} is a member of group ${group}")
  (%run-sql #?"ALTER GROUP ${group} ADD USER ${user}"
            :unless #?"select 'yes' from pg_auth_members m, pg_authid u, pg_authid g
                       where u.rolname='${user}' and g.rolname='${group}'
                       and m.member=u.oid and m.roleid=g.oid;"))

(defproplist user-can-login :posix (user)
  "Ensure USER can login to Postgres."
  (:desc #?"Postgres role ${user} can login to database")
  (%run-sql #?"ALTER USER ${user} WITH LOGIN;"
            :unless #?"select 'yes' from pg_authid where rolname='${user}' and rolcanlogin=true;"))
