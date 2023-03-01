include ../consfigurator.mk

LISP = $(wildcard ../src/property/*.lisp ../src/util/*.lisp) \
	../src/connection.lisp ../src/property.lisp ../src/propspec.lisp \
	../src/host.lisp ../src/combinator.lisp ../src/deployment.lisp \
	../src/data.lisp ../src/image.lisp ../src/util.lisp
PAGES = $(patsubst ../src/%,%,$(LISP:lisp=rst))

.PHONY: all
all: html info

.PHONY: html info
html info: $(PAGES) conf.py $(wildcard *.rst */*.rst)
	sphinx-build -M $@ . _build

$(PAGES) &: $(wildcard *.rst.in */*.rst.in) $(LISP)
	$(SBCL) --eval "(mapc #'consfigurator::build-manual-rst \
		              uiop:*command-line-arguments*)" --quit $(PAGES)

.PHONY: clean
clean:
	rm -rf _build
	rm -f $(PAGES)

# property.lisp contains the definition of BUILD-MANUAL-RST.
.SECONDEXPANSION:
%.rst: $$(wildcard $$*.rst.in) ../src/$$(*D)/$$(*F).lisp ../src/property.lisp
	$(SBCL) --eval '(consfigurator::build-manual-rst "$@")' --quit
