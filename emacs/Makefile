SUBSTITUTE = 'BEGIN { open FH, "<", "put-forms.el"; \
		chomp($$forms = join "", map s/^/  /r, grep /^\(put/, <FH>) } \
	s/  \@putforms@/$$forms/'

consfigurator.el: consfigurator.el.in put-forms.el
	perl -wpe$(SUBSTITUTE) consfigurator.el.in >consfigurator.el
