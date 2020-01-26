# @(#)Makefile	1.10 11/11/92

include MakeParams

OBJS = package.$(BIN) metainfo.$(BIN) globals.$(BIN) OS-dependent.$(BIN) \
       classes.$(BIN) framework.$(BIN) motif-widgets.$(BIN) dialogs.$(BIN) \
       background.$(BIN) views.$(BIN) commands.$(BIN) browser-shell.$(BIN) \
       browser-subclass-dialog.$(BIN) browser.$(BIN)

all: $(OBJS)

%.$(BIN): %.lisp
	@echo The file $@ is not up to date. Call make $(LISP_SYSTEM).
	@exit 1

excl: 
	cd ../clm/lisp; make excl_links
	@echo \(chdir \"../clm/lisp\"\) \(load \"load\"\) \
              \(chdir \"../../gina\"\) \(load \"load\"\) \
              \(chdir \"../gina-demos\"\) \(load \"load\"\) \
              \(chdir \"../ib\"\) \(load \"load\"\) | $(LISP) -batch

lucid:
	cd ../clm/lisp; make lucid_links
	@rm -f lucid-load-all-of-gina.lisp
	@echo \(cd \"../clm/lisp\"\) \(load \"load\"\) \(cd \"../../gina\"\) \
              \(load \"load\"\) \(cd \"../gina-demos\"\) \(load \"load\"\) \
              \(cd \"../ib\"\) \(load \"load\"\) > lucid-load-all-of-gina.lisp
	@$(LISP) -l lucid-load-all-of-gina.lisp -q
	@rm -f lucid-load-all-of-gina.lisp

install: $(LISP_SYSTEM)_install

$(LISP_SYSTEM)_install: $(OBJS)
	-@mkdir $(GINA_LIB)
	cat  $(OBJS) > $(GINA_LIB)/gina.$(BIN)

clean: 
	rm -f *.fasl *.sbin *.lbin *.sparcf *.rbin

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

