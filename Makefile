# jasql

.POSIX:
.SUFFIXES:
.SUFFIXES:  .asd .lisp .txt

VERSION=$(shell cat version)
JASQL=jasql
PACKAGE=jasql-$(VERSION)

# variables
DOTEMACS=$(HOME)/.emacs.d/init.el

ASDSRCS=$(wildcard *.asd)
LISPSRCS=$(wildcard *.lisp)
SRCS=$(ASDSRCS) $(LISPSRCS)

LISPFLAGS=--no-userinit --non-interactive --noprint

# paths
scrdir=.

prefix=/usr/local
exec_prefix=$(prefix)
bindir=$(exec_prefix)/bin
libdir=$(exec_prefix)/lib
libexecdir=$(exec_prefix)/libexec/$(JASQL)
lispdir=$(exec_prefix)/lisp/$(JASQL)

datarootdir=$(prefix)/share
datadir=$(datarootdir)/$(JASQL)
docdir=$(datarootdir)/doc/$(JASQL)
infodir=$(datarootdir)/info

# programs
INSTALL=/usr/bin/install
EMACS=/usr/local/bin/emacs
LISP=/usr/local/bin/sbcl
LS=/usr/local/bin/gls

clean:
	-rm **/*.fasl

distclean: clean
	-rm -rf $(PACKAGE).tar.gz

dist: distclean
	mkdir -p $(PACKAGE)
	cp -R $(shell $(LS)) $(PACKAGE)
	tar czf $(PACKAGE).tar.gz $(PACKAGE)
	rm -rf $(PACKAGE)

install: installdirs
	cp -R $(shell $(LS)) $(DESTDIR)$(lispdir)
	cp README.txt $(DESTDIR)$(docdir)

uninstall:
	-rm -rf $(lispdir)
	-rm -rf $(docdir)

installdirs:
	mkdir -p $(DESTDIR)$(lispdir)
	mkdir -p $(DESTDIR)$(docdir)

info:

README.txt: doc/README.org
	$(EMACS) --batch -l $(DOTEMACS) --visit $< -f org-ascii-export-to-ascii
	mv doc/README.txt .

check:
	$(LISP) $(LISPFLAGS) \
	--eval "(require 'asdf)" \
	--eval "(push *default-pathname-defaults* asdf:*central-registry*)" \
	--eval '(asdf:load-system "jasql")' \
	--eval '(setf asdf-user:*test-interactive* t)' \
	--eval '(asdf:test-system "jasql")'

.PHONY: all clean distclean dist install installdirs uninstall info check
