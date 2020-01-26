# @(#)Makefile	1.18 9/8/93
SHELL=/bin/sh

# This is the Lisp system you're using

# Allegro 4.1
LISP_SYSTEM = excl
LISP = /vol/allegro-4.1/bin/allegro-small
# Uncomment this if you do not need CLX support in CLM under Allegro
USE_CLX = (require :clx)

# Lucid 4.0
#LISP_SYSTEM = lucid
#LISP = /vol/lisp4.0/lisp-4-0-base

# Austin Kyoto Common Lisp 1.600
#LISP_SYSTEM = akcl
#LISP = /pubbay/sun4/bin/akcl

# This is the path where the compiled code for CLM should be installed.
CLM_LIB = ../lib.sun4

# DO NOT CHANGE ANYTHING AFTER HERE
EXCL_OBJ = pkg.fasl defs.fasl excl.fasl low.fasl files.fasl \
           display.fasl convenience.fasl widgets.fasl text.fasl \
	   callbacks.fasl events.fasl transl.fasl \
           dialogs.fasl cursor.fasl color.fasl listw.fasl timers.fasl

LUCID_OBJ = pkg.sbin defs.sbin lucid.sbin low.sbin files.sbin display.sbin \
	    convenience.sbin widgets.sbin text.sbin callbacks.sbin events.sbin \
	    transl.sbin dialogs.sbin \
	    cursor.sbin color.sbin listw.sbin timers.sbin

AKCL_SRC = defs.lisp akcl.lisp low.lisp files.lisp \
	   display.lisp convenience.lisp widgets.lisp \
	   text.lisp callbacks.lisp events.lisp \
	   transl.lisp dialogs.lisp \
	   cursor.lisp color.lisp listw.lisp timers.lisp

EXCL_FOREIGN = unixsocket.o io.o exclio.o runstatus.o

LUCID_FOREIGN = unixsocket.o io.o

AKCL_FOREIGN = akclio.o unixsocket.o io.o

all: $(LISP_SYSTEM)

install: $(LISP_SYSTEM)_install

excl: 
	make excl_links
	echo '$(USE_CLX)(load "load")' | $(LISP) -batch

lucid:
	make lucid_links
	$(LISP) -l load.lisp -q

akcl:
	make akcl_links
	make akcl_io
	echo '(load "load.lisp") (bye)' | $(LISP)
		
excl_install: excl excl_cat

excl_cat:
	cat  $(EXCL_OBJ) > $(CLM_LIB)/clm.fasl
	cp $(EXCL_FOREIGN) $(CLM_LIB)

lucid_install: lucid lucid_cat

lucid_cat:
	cat  $(LUCID_OBJ) > $(CLM_LIB)/clm.sbin
	cp $(LUCID_FOREIGN) $(CLM_LIB)

akcl_install: akcl akcl_cat1
	echo '(compile-file "clm.lisp") (bye)' | $(LISP)
	make akcl_cat2

akcl_cat1:
	cat pkg.lisp > ./clm.lisp
	cat  $(AKCL_SRC) | sed -e '1,$$s,(in-package :xtk),,' \
	>> ./clm.lisp

akcl_cat2:
	mv ./clm.o $(CLM_LIB)
	cp $(AKCL_FOREIGN) $(CLM_LIB)

akcl_io:
	echo '(compile-file "akclio.lisp") (bye)' | $(LISP)
	

excl_links:
	if test ! -f unixsocket.o; then ln -s ../server/unixsocket.o . ; fi
	if test ! -f io.o; then ln -s ../server/io.o . ; fi
	if test ! -f exclio.o; then ln -s ../server/exclio.o . ; fi
	if test ! -f runstatus.o; then ln -s ../server/runstatus.o . ; fi

lucid_links:
	if test ! -f unixsocket.o; then ln -s ../server/unixsocket.o . ; fi
	if test ! -f io.o; then ln -s ../server/io.o . ; fi

akcl_links:
	if test ! -f unixsocket.o; then ln -s ../server/unixsocket.o . ; fi
	if test ! -f io.o; then ln -s ../server/io.o . ; fi

clean:
	rm -f *.fasl *.sbin *.lbin *.sparcf *.rbin *.o

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

diffs: 
	@for f in SCCS/s.* ; do \
            n=`expr $$f : "SCCS/s.\(.*\)"` ;\
            if [ -f $$n ] ; then \
               sccs diffs $$n ; \
	    else \
	       echo "" ; \
	       echo "------- $$n -------" ; \
	       echo "Not in .:" $$f ; \
            fi ; \
         done
