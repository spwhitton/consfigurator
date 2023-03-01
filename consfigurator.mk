SBCL = sbcl --disable-debugger --eval '(require "asdf")' --eval \
	'(let ((asdf:*user-cache* "/tmp") \
	       (asdf:*central-registry* (list (truename "..")))) \
	   (asdf:load-system "consfigurator"))'
