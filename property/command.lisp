(in-package :consfigurator.property.command)

(defprop shell-command :posix (cmd args &key environment)
  "A property which can be applied by running a shell command.

Keyword argument :environment is a plist of environment variables to be set
when running the command, using env(1)."
  (:apply (when environment
	    (let ((env (cons "env"
			     (loop for (var . val) in environment
				   collect (concat (symbol-name var)
						   "="
						   val)))))
	      (setq args (nconc env args))))
	  (connection-run (apply #'shellcmd cmd args))))
