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
;;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

(in-package :consfigurator)

(defun read-heredoc (stream char arg)
  "Like CL-HEREDOC:READ-HEREDOC but treat #>EOF> and #>>EOF>> differently:
#>>EOF>> skips over the remainder of the current line and its newline.
For the sake of future extension, the remainder of the line after the #>>EOF>>
should not contain anything other than a single-line comment."
  (if (char= (peek-char nil stream t :eof t) char)
      ;; #>>EOF>> -- ignore the rest of the line.
      (progn (read-char stream t :eof t)
             (let* ((delim (make-string 2 :initial-element char))
                    (ender (cl-heredoc:read-until-match stream delim)))
               (read-line stream t :eof t)
               (cl-heredoc:read-until-match stream ender)))
      ;; #>EOF> -- just use the normal READ-HEREDOC.
      (cl-heredoc:read-heredoc stream char arg)))

(named-readtables:defreadtable :consfigurator
  (:merge :standard)
  (:dispatch-macro-char #\# #\? #'cl-interpol:interpol-reader)
  (:dispatch-macro-char #\# #\> #'read-heredoc))

(named-readtables:defreadtable :consfigurator.without-read-eval
  (:merge :consfigurator)
  (:dispatch-macro-char #\# #\. (constantly nil)))
