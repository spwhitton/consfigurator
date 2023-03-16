;;; Consfigurator -- Lisp declarative configuration management system

;;; Copyright (C) 2021, 2023  Sean Whitton <spwhitton@spwhitton.name>

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

(in-package :consfigurator)

(defun lines (text &optional trimfun (trimchars '(#\Space #\Tab)))
  (with-input-from-string (stream text)
    (let (bolp buffer)
      (flet ((trim (line)
               (if trimfun (funcall trimfun trimchars line) line))
             (reset ()
               (setq bolp t
                     buffer (make-array 0 :fill-pointer 0
                                          :element-type 'character))))
        ;; Split on either <CR>, <LF> or <CR><LF>; <LF><CR> would mean split
        ;; with a blank line in between.  Drop a single trailing blank line.
        (loop initially (reset)
              for char = (read-char stream nil nil)
              if char
                if (member char '(#\Return #\Newline) :test #'char=)
                  collect (trim buffer)
                  and do (reset)
                         (when (char= char #\Return)
                           (when-let ((next (peek-char nil stream nil nil)))
                             (when (char= next #\Newline)
                               (read-char stream))))
                else do (setq bolp nil)
                        (vector-push-extend char buffer)
                end
              else
                unless bolp collect (trim buffer) end
                and do (loop-finish))))))

(defun unlines (lines)
  (format nil "~{~A~%~}" lines))

(defun read-heredoc (stream char arg)
  "Like CL-HEREDOC:READ-HEREDOC, with some additional features.

Treat #>EOF> and #>>EOF>> differently: #>>EOF>> skips over the remainder of
the current line and its newline.  For the sake of future extension, the
remainder of the line after the #>>EOF>> must not contain anything other than
a single-line comment.

Preceding the specification of the terminating string with a tilde means an
indented heredoc; see perlop(1)."
  (declare (ignore arg))
  (let* ((>> (and (char= char (peek-char nil stream t :eof t))
                  (read-char stream t :eof t)))
         (indented (and (char= #\~ (peek-char nil stream t :eof t))
                        (read-char stream t :eof t)))
         (delim (if >> (make-string 2 :initial-element char) (string char)))
         (ender (cl-heredoc:read-until-match stream delim)))
    (when >>
      (read-line stream t :eof t))
    (let ((heredoc (cl-heredoc:read-until-match stream ender)))
      (if indented
          (loop with lines = (lines heredoc)
                with indent = (length (lastcar lines))
                for (line . rest) on lines while rest
                collect (subseq line (min indent (length line))) into accum
                finally (return (unlines accum)))
          heredoc))))

;; Originally inspired by some ideas of Doug Hoyt in his *Let Over Lambda*.
(defun perl-tilde-reader (stream char arg)
  (flet ((readerr (&rest args)
           (apply #'simple-reader-error stream args)))
    (let ((negated (and (char= #\! char)
                        (or (char= #\~ (read-char stream t :eof t))
                            (readerr "Expected \"~~\" following \"!\"."))))
          first-delim op)
      (loop for next = (read-char stream t :eof t)
            while (alpha-char-p next) collect next into accum
            finally (setq first-delim next
                          op (cond ((null accum) #\m)
                                   ((= (length accum) 1) (car accum))
                                   ((string= (coerce accum 'string) "tr") #\y)
                                   (t (readerr "Unknown regexp operator ~S."
                                               (coerce accum 'string))))))
      (let* ((cl-interpol:*inner-delimiters*
               (and (not (eql first-delim #\'))
                    ;; This is the same as CL-INTERPOL's regexp mode, but we
                    ;; don't turn that on for SECOND-ARG.
                    '((#\{ . #\}))))
             (first-arg
               (cl-interpol:interpol-reader
                (make-concatenated-stream
                 (make-string-input-stream (format nil "r~A" first-delim))
                 stream)
                #\? nil))
             (second-delim
               (and (member op '(#\s #\y))
                    (if (atom (find first-delim cl-interpol:*outer-delimiters*
                                    :key #'ensure-car))
                        first-delim
                        (read-char stream t :eof t))))
             (second-closer
               (aand (find second-delim cl-interpol:*outer-delimiters*
                           :key #'ensure-car)
                     (consp it) (cdr it)))
             (second-arg
               ;; Here we have to do our own pass before calling
               ;; CL-INTERPOL:INTERPOL-READER because we need to additionally
               ;; preserve \`, \& and \', and its regexp mode doesn't do that.
               ;; We do want to use CL-INTERPOL:INTERPOL-READER too, because
               ;; interpolating into replacement strings is useful.
               (and second-delim
                    (loop
                      with escaped and depth = 0
                      and result = (make-array 1 :fill-pointer 1
                                                 :element-type 'character
                                                 ;; Use our own delimiter to
                                                 ;; ensure no regexp mode.
                                                 :initial-element #\#)
                      for next = (read-char stream t :eof t)
                      for extra-escape = (if escaped
                                             (or (digit-char-p next)
                                                 (member next '(#\` #\& #\')))
                                             (eql next #\#))
                      if escaped do (setq escaped nil)
                        else do (switch (next)
                                  (second-delim (if second-closer
                                                    (incf depth)
                                                    (loop-finish)))
                                  (second-closer (if (zerop depth)
                                                     (loop-finish)
                                                     (decf depth)))
                                  (#\\ (setq escaped t)))
                      when extra-escape
                        do (vector-push-extend #\\ result)
                      do (vector-push-extend next result)
                      finally (vector-push-extend #\# result)
                              (return (cl-interpol:interpol-reader
                                       (make-string-input-stream result)
                                       #\? nil)))))
             (modes (loop for next = (read-char stream t :eof t)
                          while (alpha-char-p next) collect next
                          finally (unread-char next stream)))
             (try-parse (find #\p modes))
             (scanner-args
               (list first-arg
                     :case-insensitive-mode (and (find #\i modes) t)
                     :multi-line-mode (and (find #\m modes) t)
                     :single-line-mode (and (find #\s modes) t)
                     ;; We're choosing not to use CL-INTERPOL's own extended
                     ;; mode because CL-PPCRE's is closer to Perl's.
                     :extended-mode (and (find #\x modes) t)))
             (scanner (if (constantp first-arg)
                          `(load-time-value
                            (re:create-scanner ,@scanner-args))
                          `(re:create-scanner ,@scanner-args)))
             (body
               (ecase op
                 (#\m
                  (cond ((member #\g modes)
                         (let ((form
                                 `(re:all-matches-as-strings ,scanner
                                                             target-string)))
                           (if try-parse
                               `(aand ,form
                                      (map-into it #'try-parse-number it))
                               form)))
                        ((not arg)
                         ;; The number of capture groups is constant if
                         ;; FIRST-ARG is constant, so could we self-replace
                         ;; on first execution / otherwise memoise?
                         `(multiple-value-bind (zeroth rest)
                              (re:scan-to-strings ,scanner target-string)
                            ;; We could (coerce rest 'list) for use with
                            ;; DESTRUCTURING-BIND.  But there is already
                            ;; CL-PPCRE:REGISTER-GROUPS-BIND.
                            (if (zerop (length rest))
                                ,(if try-parse
                                     '(try-parse-number zeroth)
                                     'zeroth)
                                ,(if try-parse
                                     '(map-into rest #'try-parse-number rest)
                                     'rest))))
                        ((zerop arg)
                         (let ((form `(re:scan-to-strings ,scanner
                                                          target-string)))
                           (if try-parse
                               `(multiple-value-bind (match groups) ,form
                                  (values (try-parse-number match)
                                          (map-into groups #'try-parse-number
                                                    groups)))
                               form)))
                        (t              ; ARG is a positive integer
                         `(aand
                           (nth-value 1 (re:scan-to-strings ,scanner
                                                            target-string))
                           ,(if try-parse
                                `(values (try-parse-number
                                          (aref it ,(1- arg)))
                                         (map-into it #'try-parse-number it))
                                `(values (aref it ,(1- arg)) it))))))
                 (#\s `(,(if (member #\g modes)
                             're:regex-replace-all
                             're:regex-replace)
                        ,scanner target-string ,second-arg))
                 (#\y (readerr "Transliteration unimplemented.")))))
        `(lambda (target-string)
           ,(if negated `(not ,body) body))))))

(named-readtables:defreadtable :consfigurator
  (:merge :standard)
  (:dispatch-macro-char #\# #\? #'cl-interpol:interpol-reader)
  (:dispatch-macro-char #\# #\> #'read-heredoc)

  (:dispatch-macro-char #\# #\~ #'perl-tilde-reader)
  (:dispatch-macro-char #\# #\! #'perl-tilde-reader))

(named-readtables:defreadtable :consfigurator.without-read-eval
  (:merge :consfigurator)
  (:dispatch-macro-char #\# #\. (constantly nil)))
