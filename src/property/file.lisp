;;; Consfigurator -- Lisp declarative configuration management system

;;; Copyright (C) 2020-2021  Sean Whitton <spwhitton@spwhitton.name>

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

(in-package :consfigurator.property.file)
(named-readtables:in-readtable :consfigurator)

(defun map-file-lines (file function)
  "Apply FUNCTION to the lines of FILE.  Safe to use in a :POSIX property.

For efficiency, a :LISP property might want to use streams, but there's no
point in doing that here because WRITEFILE is synchronous."
  (let* ((orig-lines (and (remote-exists-p file) (lines (readfile file))))
         (new-lines (funcall function orig-lines)))
    (if (equal orig-lines new-lines)
        :no-change
        (writefile file (unlines new-lines)))))

(defprop has-content :posix (path content)
  "Ensure there is a file at PATH whose content is CONTENT.
CONTENT can be a list of lines or a single string."
  (declare (indent 1))
  (:desc (declare (ignore content))
         #?"${path} has defined content")
  (:apply (with-change-if-changes-file-content (path)
            (writefile path (etypecase content
                              (cons (unlines content))
                              (string (format nil "~A~&" content)))))))

(defprop contains-lines :posix (path lines)
  "Ensure there is a file at PATH containing each of LINES once."
  (:apply
   (with-change-if-changes-file-content (path)
     (let ((new-lines (copy-list (ensure-cons lines)))
           (existing-lines (lines (readfile path))))
       (dolist (existing-line existing-lines)
         (deletef new-lines existing-line :test #'string=))
       (writefile path (unlines (nconc existing-lines new-lines)))))))

(defprop has-mode :posix (path mode)
  "Ensure that a file has a particular numeric mode."
  (:desc (format nil "~A has mode ~O" path mode))
  (:apply
   (with-change-if-changes-file (path)
     (mrun (format nil "chmod ~O ~A" mode path)))))

(defprop does-not-exist :posix (&rest paths)
  "Ensure that files do not exist."
  (:desc (if (cdr paths)
             #?"@{paths} do not exist"
             #?"${(car paths)} does not exist"))
  (:check (remote-exists-p paths))
  (:apply (mrun "rm" "-f" paths)))

(defprop data-uploaded :posix (iden1 iden2 destination)
  (:hostattrs
   (declare (ignore destination))
   (require-data iden1 iden2))
  (:apply
   (with-change-if-changes-file-content (destination)
     (writefile destination (get-data-stream iden1 iden2)))))

(defprop host-data-uploaded :posix (destination)
  (:hostattrs
   (require-data (get-hostname) destination))
  (:apply
   (data-uploaded (get-hostname) destination destination)))

(defprop secret-uploaded :posix (iden1 iden2 destination)
  (:hostattrs
   (declare (ignore destination))
   (require-data iden1 iden2))
  (:apply
   (with-change-if-changes-file-content (destination)
     (writefile destination (get-data-stream iden1 iden2) :mode #o600))))

(defprop host-secret-uploaded :posix (destination)
  (:hostattrs
   (require-data (get-hostname) destination))
  (:apply
   (secret-uploaded (get-hostname) destination destination)))

(defprop regex-replaced-lines :posix (file regex replace)
  "Like s/REGEX/REPLACE/ on the lines of FILE.
Uses CL-PPCRE:REGEX-REPLACE, which see for the syntax of REPLACE."
  (:apply
   (map-file-lines
    file
    (lambda (lines)
      (mapcar (lambda (line) (re:regex-replace regex line replace)) lines)))))

(defprop directory-exists :posix (dir)
  "Ensure that a directory and its parents exists."
  (:desc (strcat dir " exists"))
  (:apply
   (mrun "mkdir" "-p" dir)
   ;; assume it was already there
   :no-change))


;;;; Config files

;; all our actual uses of this so far could avoid traversing the lines of the
;; file more than once if this function instead applied MAP to each line and
;; then just transformed it back right away, but in the future we might want
;; to write functions which operate on the whole set of lines at once
(defun config-file-map
    (&key (parse-comment "#") (new-comment "# ")
       (parse-section (constantly nil)) (new-section #'identity)
       parse-kv new-kv map)
  "Return a function suitable for passing to FILE:MAP-FILE-LINES, to modify
the lines of a config file using MAP.  MAP is a function from a list of config
file lines to a list of config file lines, except that lines which set values
in the original file will be replaced by lists of the form (COMMENTED SECTION
KEY VALUE), where

    - COMMENTED is a boolean indicating whether the line was commented
    - SECTION is the section of the config file in which the line appears

and KEY and VALUE are the key and value.  MAP may return lists of this form
and they will be converted back into strings.

Other arguments:

- PARSE-COMMENT is a CL-PPCRE regular expression which, when matched at the
  beginning of a line, indicates a comment.  It is assumed that it can be
  repeated and may be followed by whitespace.

- NEW-COMMENT is a string to be prepended to lines to comment them out.

- PARSE-SECTION is a function which returns the name of the section if passed
  a line which begins a section of the config file, or nil if the line does
  not start a section.  It can also be a CL-PPCRE regexp, which should extract
  the section name as the first capture group.  Lines will be passed to this
  function (or matched against this regexp) uncommented.

- NEW-SECTION is a function which takes a section name and returns a line
  (without trailing newline) beginning a new section with that name.

- PARSE-KV is a function which returns as a cons the key and value set by a
  line of the config file, or nil if the line is something else.  It can also
  be a CL-PPCRE regexp, which should extract the key and value as the first
  and second capture groups, respectively.  Lines will be passed to this
  function (or matched against this regexp) uncommented.

- NEW-KV is a function of two arguments, a key and a value, which returns an
  uncommented line setting the key and value."
  (unless (functionp parse-section)
    (let ((orig parse-section))
      (setq parse-section (lambda (line)
                            (multiple-value-bind (match groups)
                                (re:scan-to-strings orig line)
                              (when match (elt groups 0)))))))
  (unless (functionp parse-kv)
    (let ((orig parse-kv))
      (setq parse-kv (lambda (line)
                       (multiple-value-bind (match groups)
                           (re:scan-to-strings orig line)
                         (and match (cons (elt groups 0) (elt groups 1))))))))
  (flet ((uncomment (line)
           (multiple-value-list
            (re:regex-replace #?/^(?:${parse-comment})+\s*/ line ""))))
    (lambda (lines)
      (let* ((unmapped
               (loop with current-section
                     for line in lines
                     for (uncommented commentedp) = (uncomment line)
                     for (k . v) = (funcall parse-kv uncommented)
                     for new-section
                       = (and (not commentedp)
                              (funcall parse-section uncommented))
                     do (setq current-section
                              (or new-section current-section))
                     if (and k v)
                       collect (list commentedp current-section k v)
                     else collect line))
             (mapped (funcall map unmapped)))
        (loop with current-section
              for line in mapped
              for line-section = (etypecase line
                                   (cons (cadr line))
                                   (string (funcall parse-section line)))

              if (and (listp line)
                      line-section
                      (not (string= line-section current-section)))
                collect ""
                and collect (funcall new-section line-section)
                and do (setq current-section line-section)
              else if (and (stringp line)
                           line-section
                           (not (string= line-section current-section)))
                     do (setq current-section line-section)

              if (listp line)
                collect (with-output-to-string (s)
                          (destructuring-bind (commentedp sec k v) line
                            (declare (ignore sec))
                            (when commentedp (princ new-comment s))
                            (princ (funcall new-kv k v) s)))
              else collect line)))))

(defun simple-conf-update (file pairs &rest args)
  (let ((keys (make-hash-table :test #'equal)))
    (loop for (k v) on pairs by #'cddr
          unless (stringp v)
            do (simple-program-error
                "Values passed are not all strings, or list is not even")
          do (setf (gethash k keys) v))
    (map-file-lines
     file (apply
           #'config-file-map
           :map
           (lambda (lines)
             (let ((new-lines
                     (loop for line in lines
                           for key = (and (listp line) (caddr line))
                           for val = (and (listp line) (gethash key keys))
                           if (eql val :done)
                             collect (list* t nil (cddr line))
                           else if val
                                  collect (list nil nil key val)
                                  and do (setf (gethash key keys) :done)
                           else collect line)))
               (loop for k being the hash-keys in keys using (hash-value v)
                     unless (eql v :done)
                       collect (list nil nil k v) into accum
                     finally (return (nconc new-lines accum)))))
           args))))

(defprop contains-conf-space :posix (file &rest pairs)
  "Where FILE is a config file in which keys and values are separated by spaces
and there are no sections, and PAIRS is a list of even length of alternating
keys and values, set each of these keys and values in FILE.

If there are any other lines which set values for the same keys, they will be
commented out; the first commented or uncommented line for each key will be
uncommented and used to set the value, if it exists."
  (:desc (format nil "~A has ~{~A ~A~^, ~}" file pairs))
  (:apply (simple-conf-update file pairs
                              :parse-kv #?/^(\S+) (.+)/
                              :new-kv (lambda (k v) #?"${k} ${v}"))))

(defprop contains-conf-equals :posix (file &rest pairs)
  "Where FILE is a config file in which keys and values are separated by \" = \"
and there are no sections, and PAIRS is a list of even length of alternating
keys and values, set each of these keys and values in FILE.

If there are any other lines which set values for the same keys, they will be
commented out; the first commented or uncommented line for each key will be
uncommented and used to set the value, if it exists."
  (:desc (format nil "~A has ~{~A = ~A~^, ~}" file pairs))
  (:apply (simple-conf-update file pairs
                              :parse-kv #?/^(\S+) = (.+)/
                              :new-kv (lambda (k v) #?"${k} = ${v}"))))

(defprop contains-conf-shell :posix (file &rest pairs)
  "Where FILE is a shell config file, like those in /etc/default, and PAIRS is a
list of even length of alternating keys and values, set each of these keys and
values in FILE.

If there are any other lines which set values for the same keys, they will be
commented out; the first commented or uncommented line for each key will be
uncommented and used to set the value, if it exists."
  (:desc (format nil "~A has ~{~A=~S~^, ~}" file pairs))
  (:apply (simple-conf-update file (loop for (k v) on pairs by #'cddr
                                         collect k
                                         collect (escape-sh-token v))
                              ;; include quoting as part of the value so we
                              ;; don't end up substituting double quotation
                              ;; marks for single, or similar
                              :parse-kv #?/^(\S+)\s?=\s?(.*)/
                              :new-kv (lambda (k v) #?"${k}=${v}"))))

(defprop contains-ini-settings :posix (file &rest triples)
  "Where FILE is an INI file, and each of TRIPLES is a list of three elements,
a section name, key and value, set those keys and values in FILE.  Keys
containing '=' are not supported.

If there are any other lines which set values for the same keys in the same
sections, they will be commented out.  The first commented or uncommented line
for each key in its section will be uncommented and used to set the value, if
it exists.

Some normalisation will be performed: whitespace around the '=' will be
removed, and semicolon comment chars will be replaced with '#'."
  (:desc (format nil "~A has ~{~{[~A] ~A = ~A~}~^, ~}" file triples))
  (:apply
   (let ((parse-section #?/^\[(.+)\]$/)
         (keys (make-hash-table :test #'equal)))
     (loop for (s k v) in triples
           do (setf (gethash (cons s k) keys) v))
     (map-file-lines
      file
      (config-file-map
       :parse-comment "[#;]"
       :parse-section parse-section
       :new-section (lambda (s) #?"[${s}]")
       :parse-kv #?/^([^=\s]+)\s*=\s*(.*)/
       :new-kv (lambda (k v) #?"${k}=${v}")
       :map
       (lambda (lines)
         (let ((new-lines
                 (loop with current-section
                       for line in lines
                       for sec = (etypecase line
                                   (list (cadr line))
                                   (string (re:regex-replace
                                            parse-section line #?/\1/)))
                       and key = (and (listp line) (caddr line))
                       for pair = (cons sec key)
                       for val = (and (listp line) (gethash pair keys))

                       ;; If we've reached a new section insert any
                       ;; remaining pairs in this section, as that's
                       ;; better than inserting a new section with the
                       ;; same name at the end of the file.
                       if (and sec (not (string= sec current-section)))
                         nconc (loop for pair being the hash-keys in keys
                                       using (hash-value v)
                                     for (s . k) = pair
                                     when (and (string= current-section s)
                                               (not (eql v :done)))
                                       collect (list nil s k v)
                                       and do (setf (gethash pair keys) :done))
                         and do (setq current-section sec)

                       if (eql val :done)
                         collect (cons t (cdr line))
                       else if val
                              collect (list nil sec key val)
                              and do (setf (gethash pair keys) :done)
                       else collect line)))
           (loop for pair being the hash-keys in keys using (hash-value v)
                 for (s . k) = pair
                 unless (eql v :done)
                   collect (list nil s k v) into accum
                 finally (return
                           (nconc new-lines
                                  (sort accum #'string< :key #'cadr)))))))))))
