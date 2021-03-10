LOAD = '(let ((asdf:*user-cache* "/tmp") \
              (asdf:*central-registry* (list (truename "..")))) \
          (asdf:load-system "consfigurator"))'

SUBSTITUTE = 'BEGIN { open FH, "<", "put-forms.el"; \
		chomp($$forms = join "", map s/^/  /r, grep /^\(put/, <FH>) } \
	s/  \@putforms@/$$forms/'

consfigurator.el: consfigurator.el.in put-forms.el
	perl -wpe$(SUBSTITUTE) consfigurator.el.in >consfigurator.el

put-forms.el:
	sbcl --disable-debugger --eval '(require "asdf")' --eval $(LOAD) \
	--eval '(consfigurator::dump-properties-for-emacs "$@")' --quit
