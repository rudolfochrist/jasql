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
srcdir=$(PWD)

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
LISP=/usr/local/bin/sbcl
LS=/usr/local/bin/gls
MAKEINFO=/usr/local/bin/makeinfo

clean:
	-rm **/*.fasl
	-rm -rf doc/dict.texi doc/include

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

info: jasql.info

jasql.info: doc/jasql.texi doc/dict.texi
	$(MAKEINFO) $(srcdir)/doc/jasql.texi

doc/dict.texi: $(SRCS)
	$(LISP) $(LISPFLAGS) \
	--eval "(require 'asdf)" \
	--eval '(push *default-pathname-defaults* asdf:*central-registry*)' \
	--eval '(asdf:load-system "jasql")' \
	--eval '(asdf:load-system "sb-texinfo")' \
	--eval '(sb-texinfo:document-package :jasql :output-file "doc/dict.texi" :standalone nil :write-backmatter nil :write-menu nil :exclude-node t)'

check:
	$(LISP) $(LISPFLAGS) \
	--eval "(require 'asdf)" \
	--eval "(push *default-pathname-defaults* asdf:*central-registry*)" \
	--eval '(asdf:load-system "jasql")' \
	--eval '(setf asdf-user:*test-interactive* t)' \
	--eval '(asdf:test-system "jasql")'

.PHONY: all clean distclean dist install installdirs uninstall info check
