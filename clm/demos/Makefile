# @(#)Makefile	1.9 1/29/92

# This is the Lisp system you're using

# Allegro 3.0.1
LISP_SYSTEM = excl
LISP = /vol/allegro4.0/bin/allegro-small

# Lucid 3.0
#LISP_SYSTEM = lucid
#LISP = /vol/lisp4.0/lisp-4-0-base

# This is the path where the compiled code for CLM should be installed.
CLM_LIB = ../lib.sun4

#Do not change anything after here

EXCL_OBJ = demo.fasl

LUCID_OBJ = demo.sbin

all: $(LISP_SYSTEM)

install: $(LISP_SYSTEM)_install

excl:
	echo '(load "load")' | $(LISP) -batch

lucid:
	$(LISP) -l load -q

excl_install: excl excl_cat

excl_cat:
	cat  $(EXCL_OBJ) > $(CLM_LIB)/clm-demos.fasl

lucid_install: lucid lucid_cat

lucid_cat:
	cat  $(LUCID_OBJ) > $(CLM_LIB)/clm-demos.sbin

clean:
	rm -f *.fasl *.sbin *.lbin

sccs: 
	@for f in SCCS/s.* ; do \
            n=`expr $$f : "SCCS/s.\(.*\)"` ;\
            if [ ! -f $$n ] ; then \
               echo $$f ; \
               sccs get $$n ; \
            fi ; \
            find $$f -newer $$n -print -exec sccs get $$n \; ; \
         done
	@echo "Edited files: "
	@sccs tell -u
	@if [ "`sccs tell -u`" ] ; then sccs delget `sccs tell -u` ; fi

