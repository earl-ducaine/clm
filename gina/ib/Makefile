# @(#)Makefile	1.6 9/15/92

include ../gina/MakeParams

OBJS = pkg.$(BIN) classes.$(BIN) documents.$(BIN) counter.$(BIN) \
       resources.$(BIN) infos.$(BIN) constraints.$(BIN) class-tree.$(BIN) \
       main-view.$(BIN) proto-view.$(BIN) plates.$(BIN) info-cmds.$(BIN) \
       mouse-cmds.$(BIN) tree-cmds.$(BIN) lisp-code.$(BIN) cplus-code.$(BIN) \
       test-code.$(BIN) main-window.$(BIN) coder-window.$(BIN) \
       tree-window.$(BIN) dialogs.$(BIN) color.$(BIN) widget-drag.$(BIN) \
       grabber.$(BIN) interface.$(BIN) form-view.$(BIN) form-cmds.$(BIN) \
       prefs.$(BIN) ib-demo.$(BIN) demo-view.$(BIN)

all: $(OBJS)

%.$(BIN): %.lisp
	@echo The file $@ is not up to date. Run \(load \"load\"\) in Lisp.
	@exit 1

install: $(LISP_SYSTEM)_install

$(LISP_SYSTEM)_install: $(OBJS)
	-@mkdir $(GINA_LIB)
	cat  $(OBJS) > $(GINA_LIB)/ib.$(BIN)

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
