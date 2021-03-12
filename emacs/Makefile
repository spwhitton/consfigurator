LOAD = '(let ((asdf:*user-cache* "/tmp") \
              (asdf:*central-registry* (list (truename "..")))) \
          (asdf:load-system "consfigurator"))'

consfigurator.el: consfigurator.el.in
	sbcl --disable-debugger --eval '(require "asdf")' --eval $(LOAD) \
	--eval '(consfigurator::dump-properties-for-emacs "$<" "$@")' --quit
