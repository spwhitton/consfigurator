(in-package :consfigurator.property.file)

(defprop file-has-content :posix (path lines)
  "Ensure there is a file at PATH whose lines are the elements of LINES."
  (:apply (writefile path (unlines lines))))

(defprop file-contains-lines :posix (path lines)
  "Ensure there is a file at PATH containing each of LINES."
  (:apply (let ((new-lines (copy-list lines))
		(existing-lines (lines (readfile path))))
	    (loop for existing-line in existing-lines
		  do (setq new-lines (delete existing-line new-lines)))
	    (writefile path (unlines
			     (nconc existing-lines new-lines))))))
