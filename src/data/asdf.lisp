(in-package :consfigurator.data.asdf)

(defmethod register-data-source ((type (eql :asdf)) &key)
  (add-data-source #'asdf-data-source-check #'get-path-to-concatenated-system))

(defun asdf-data-source-check (iden1 system)
  (and (string= iden1 "lisp-system")
       (asdf:find-system system nil)))

(Defun get-path-to-concatenated-system (iden1 system)
  "Try to concatenate all the source code for SYSTEM, store it somewhere and
return the filename."
  (let ((cache-dir (uiop:ensure-pathname-directory
		    (strcat (or (uiop:getenv "XDG_CACHE_HOME")
				(strcat (uiop:getenv "HOME") "/.cache"))
			    "/consfigurator/systems")))
	(op 'asdf:monolithic-concatenate-source-op)
	(co (asdf:find-component system nil)))
    (ensure-directories-exist cache-dir)
    (asdf:initialize-output-translations `(:output-translations
					   (t ,cache-dir)
					   :disable-cache
					   :ignore-inherited-configuration))
    (asdf:operate op co)
    (list :file (asdf:output-file op co))))
