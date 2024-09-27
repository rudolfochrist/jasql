.POSIX:
.SUFFIXES:
.SUFFIXES:  .asd .lisp .txt

VERSION=$(shell cat version)
JASQL=jasql
PACKAGE=$(JASQL)-$(VERSION)

# variables
ASDSRCS=$(wildcard **/*.asd)
LISPSRCS=$(wildcard **/*.lisp)
SRCS=$(ASDSRCS) $(LISPSRCS)

# sbcl
LISPFLAGS=--noinform --non-interactive --no-userinit

# paths
srcdir=$(PWD)

# programs
LISP=/opt/local/bin/sbcl
MAKEINFO=/opt/local/bin/makeinfo
