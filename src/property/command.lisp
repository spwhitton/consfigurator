(in-package :consfigurator.property.command)

(defprop shell-command :posix (cmd args &key environment)
  "A property which can be applied by running a shell command.

Keyword argument :environment is a plist of environment variables to be set
when running the command, using env(1)."
  (:apply (apply #'run-with-input nil environment cmd args)))
