# @(#)Makefile	1.7 11/11/92

include ../gina/MakeParams

OBJS = finder-shell.$(BIN) finder.$(BIN) graphic-editor.$(BIN) clock.$(BIN) \
       graphic-output.$(BIN) hello.$(BIN) lisp-widgets.$(BIN) micky.$(BIN) \
       text-editor.$(BIN) bitmap-editor.$(BIN) tetris.$(BIN) hyper.$(BIN) \
       spreadsheet.$(BIN) pacmen.$(BIN) chess.$(BIN) drag-and-drop.$(BIN) \
       mandelbrot.$(BIN) calculator.$(BIN) calculator-shell.$(BIN) \
       diagram-editor.$(BIN) $(LISTEN)

all: $(OBJS)

%.$(BIN): %.lisp
	@echo The file $@ is not up to date. Run \(load \"load\"\) in Lisp.
	@exit 1

install: $(LISP_SYSTEM)_install

$(LISP_SYSTEM)_install: $(OBJS)
	-@mkdir $(GINA_LIB)
	cat  $(OBJS) > $(GINA_LIB)/gina-demos.$(BIN)

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

