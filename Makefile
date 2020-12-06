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

all:

clean:

distclean: clean

dist: distclean

install: all installdirs

uninstall:

installdirs:

info:

README.txt: doc/README.org
	$(EMACS) --batch -l $(DOTEMACS) --visit $< -f org-ascii-export-to-ascii
	mv doc/README.txt .

check:

.PHONY: all clean distclean dist install installdirs uninstall info check
