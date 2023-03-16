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

(named-readtables:defreadtable :consfigurator
  (:merge :standard)
  (:dispatch-macro-char #\# #\? #'cl-interpol:interpol-reader)
  (:dispatch-macro-char #\# #\> #'read-heredoc))

(named-readtables:defreadtable :consfigurator.without-read-eval
  (:merge :consfigurator)
  (:dispatch-macro-char #\# #\. (constantly nil)))
