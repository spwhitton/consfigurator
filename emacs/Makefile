include ../consfigurator.mk

consfigurator.el: consfigurator.el.in
	$(SBCL) --eval '(consfigurator::dump-properties-for-emacs "$<" "$@")'\
		--quit
