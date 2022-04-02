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

(in-package :consfigurator.connection.fork)
(named-readtables:in-readtable :consfigurator)

(defclass fork-connection (local-connection) ())

(defgeneric post-fork (connection)
  (:documentation
   "Code to execute after forking/reinvoking but before calling CONTINUE-DEPLOY*.
Must not start up any threads."))

(defmethod continue-connection ((connection fork-connection) remaining)
  (multiple-value-bind (out err exit)
      (eval-in-grandchild `(post-fork ,connection)
                          `(continue-deploy* ,connection ',remaining))
    (when-let ((lines (lines out)))
      (inform t lines))
    (return-exit
     exit
     :on-failure (failed-change
                  "~&Fork connection child failed; stderr was ~%~%~A" err))))


;;;; Dumping and then immediately reinvoking Lisp

(defclass init-hooks-connection (fork-connection) ()
  (:documentation "On SBCL, call POST-FORK using SB-EXT:*INIT-HOOKS*.

The primary purpose of this connection type is to obtain a truly
single-threaded context for the execution of POST-FORK."))

#+(and sbcl sb-thread)
(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; UIOP:VERSION< cannot handle Debian-patched SBCL version numbers, so we
  ;; split it up ourselves.
  (destructuring-bind (major minor patch . rest)
      (mapcar (lambda (s) (parse-integer s :junk-allowed t))
              (split-string (lisp-implementation-version) :separator '(#\.)))
    (declare (ignore rest))
    (unless (or (> major 2)
                (and (= major 2)
                     (or (> minor 1) (and (= minor 1) (> patch 7)))))
      (pushnew 'older-sbcl *features*))))

#+sbcl
(defmethod continue-connection ((connection init-hooks-connection) remaining)
  (multiple-value-bind (out err exit)
      (eval-in-reinvoked
       `(push
         (lambda ()
           (handler-bind
               ((serious-condition
                  (lambda (c)
                    (trivial-backtrace:print-backtrace c :output *error-output*)
                    (uiop:quit 1))))
             ;; Handle the finaliser thread in older SBCL, before the change in
             ;; 2.1.8 to call *INIT-HOOKS* before starting system threads.
             #+consfigurator.connection.fork::older-sbcl
             (sb-int:with-system-mutex (sb-thread::*make-thread-lock*)
               (sb-impl::finalizer-thread-stop))
             (post-fork ,connection)
             #+consfigurator.connection.fork::older-sbcl
             (sb-impl::finalizer-thread-start)))
         sb-ext:*init-hooks*)
       `(continue-deploy* ,connection ',remaining))
    (when-let ((lines (lines out)))
      (inform t lines))
    (return-exit
     exit
     :on-failure (failed-change
                  "~&Reinvoked Lisp image failed; stderr was ~%~%~A" err))))
