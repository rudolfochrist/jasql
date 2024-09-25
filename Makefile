# jasql

include config.mk

.PHONY:
clean:
	-rm -f **/*.fasl
	-rm -f doc/dict.texi
	-rm -rf doc/include/

.PHONY:
info: jasql.info

jasql.info: doc/jasql.texi doc/dict.texi
	$(MAKEINFO) $(srcdir)/doc/jasql.texi

doc/dict.texi: $(SRCS)
	$(LISP) $(LISPFLAGS) \
	--load init.lisp \
	--load doc/documentation.lisp

.PHONY:
check:
	$(LISP) $(LISPFLAGS) \
	--load init.lisp \
	--eval '(asdf:test-system "jasql.sqlite")' \
	--eval '(asdf:test-system "jasql.postgres")'
