# @(#)Makefile	1.11 1/13/94

TARDEVICE = CLM2.3.tar
TAR = tar

all:
	cd server; make
	cd lisp; make
	cd demos; make

install:
	cd server; make install
	cd lisp; make install
	cd demos; make install

clean:
	cd server; make clean
	cd lisp; make clean
	cd demos; make clean

tar:
	$(TAR) cvf $(TARDEVICE) Makefile README RELEASE-NOTES COPYRIGHT \
	server/COPYRIGHT server/Makefile* server/*.[ch] \
	lisp/COPYRIGHT lisp/Makefile lisp/clm.system \
        lisp/gina-host.translations lisp/*.lisp \
	demos/README demos/Makefile demos/*.lisp \
	doc/clm.tex doc/clm.PS doc/clm.dvi doc/dina4.sty doc/defs.tex \
	doc/clm.idx doc/clm.ind doc/clm.bbl doc/README doc/makeidx.sty \
	doc/showidx.sty doc/proglist.sty

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

