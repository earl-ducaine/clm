# sccsid = @(#)Makefile	1.13 11/10/92

# Architecture
# This variable determines which of the architecture-specific Makefiles
# (Makefile.<ARCH>) is included later.
ARCH=linux

#----------------------------------------------------------------------------
# The following variables are usually overridden in an architecture-specific
# file. These are the default values that may be changed for your site.

# Directories where the Motif header files and the Motif toolkit
# header files are stored.
# Make sure you're using the Motif Toolkit headers and NOT the X Toolkit headers
XM_INCLUDE = -I/usr/include/Xm
XT_INCLUDE = -I/usr/include/X11

# Directories where the include files for Allegro CL reside
EXCL_INCLUDE = -I/vol/allegro/lib/misc

# Libraries
# Make sure you're using the Motif toolkit library and NOT the X Toolkit library
LIB_PATH = -L/usr/lib

# Any additional CFLAGS
ADD_CFLAGS = -D_NO_PROTO -ggdb
# CFLAGS only for the foreign functions loaded into Lisp
LISP_CFLAGS =
# Additional flags for loader operation
ADD_LFLAGS = -O

# Any additional libraries
ADD_LIBS =

# A queer hack for RS/6000
ENUMSETGET = EnumSetGet

#----------------------------------------------------------------------------
# The following variables are site-specific

# Comment this line out if you don't compile with Allegro Lisp
LISP_OBJECTS = exclio.o runstatus.o

# Directories where CLM should be installed
# Directory where the Lisp code and the foreign function code should be inst.
CLM_LIB=../lib.$(ARCH)
# Directory where the Motif daemon and the Motif server should be installed.
CLM_BIN=../bin.$(ARCH)

# Uncomment the following lines if you use the graph widget
# The graph widget is not contained in the official release because there is
# a more restrictive copyright on it. 8-{
#XBAB_INCLUDE = -I..
#XBAB_LIB = ../Xbab/libXbab.a
#XBAB_FLAGS = -DGRAPHWIDGET

# Uncomment DEBUG_LIB if you don't compile on SUN's or if you don't want
#  to debug malloc().
#DEBUG_LIB = /usr/lib/debug/malloc.o

#----------------------------------------------------------------------------
# The platform-specific definitions are included here:
include Makefile.$(ARCH)

#----------------------------------------------------------------------------
# The following variables normally need not to be changed:

CC = gcc
CFLAGS= -ggdb -c -O $(ADD_CFLAGS) $(XBAB_FLAGS) -D$(ARCH)

LFLAGS= $(ADD_LFLAGS)

XM_LIB = -lXm
XT_LIB = -lXt
X11_LIB = -lX11

APPLICATION = \"clmd\"
CLASS = \"Clmd\"
DEST=clm-server
DAEMON=clm-td
CLM_SOCKET = \"/tmp/clm_socket\"
TCP_SOCKET = 7000
SERVER = \"$(DEST)\"
TABLE_INCLUDE = ClassTable.inc
TD_DONT_DUMP = 1
TD_RETRIES = 1

#----------------------------------------------------------------------------
# Don't change anything after here !

OBJECTS = io.o unixio.o arg.o main.o \
	  Classes.o Widgets.o Resource.o ActionMgr.o\
	  Callbacks.o GeometryMgr.o CallbackMgr.o TranslMgr.o\
	  PopupMgr.o WidgetMgr.o ResourceMgr.o AppMgr.o EventMgr.o Events.o\
	  ResourceSet.o ResourceGet.o List.o Text.o GinaView.o\
	  Protocol.o ProtocolMgr.o TypeTable.o ShellCmd.o Cursor.o Warnings.o\
	  Mwm.o Convenience.o DisplayMgr.o Graph.o Timers.o Traversal.o \
	  EnumSetGet.o MotifMisc.o Text2.o

LIBRARIES = $(DEBUG_LIB) $(XBAB_LIB) $(LIB_PATH) $(XM_LIB) $(XT_LIB) $(X11_LIB) -lm $(ADD_LIBS)

INCLUDE = $(XBAB_INCLUDE) $(XM_INCLUDE) $(XT_INCLUDE)

HEADER = interface.h functions.h opcodes.h externals.h

all: $(DEST) $(DAEMON) unixsocket.o io.o $(LISP_OBJECTS)

$(DEST): $(OBJECTS)
	$(CC) $(LFLAGS) $(OBJECTS) -o $(DEST) $(LIBRARIES)

inc-test:
	/lib/cpp -H $(INCLUDE) GenTable.c /dev/null

GenTable: GenTable.o RedTypeTable.o
	$(CC) $(LFLAGS) GenTable.o RedTypeTable.o -o GenTable $(LIBRARIES)

td.o: td.c
	$(CC) $(CFLAGS) $(INCLUDE) -DSOCKET_PORT=$(TCP_SOCKET) \
			-DSOCKET_PATH=$(CLM_SOCKET) \
	                -DSERVER_PATH=$(SERVER) \
	                -DTD_DONT_DUMP=$(TD_DONT_DUMP) \
	                -DTD_RETRIES=$(TD_RETRIES) \
			-DAPPNAME=$(APPLICATION) \
			-DAPPCLASS=$(CLASS)  td.c

$(DAEMON): td.o
	$(CC) $(LFLAGS) td.o -o $(DAEMON) $(LIBRARIES)

main.o: main.c $(HEADER)
	$(CC) $(CFLAGS) $(INCLUDE) main.c

arg.o: arg.c $(HEADER)
	$(CC) -DARG_DEBUG $(CFLAGS) $(INCLUDE) arg.c

Classes.o: Classes.c $(HEADER) $(TABLE_INCLUDE)
	$(CC) $(CFLAGS) $(INCLUDE) \
	      -DCLASS_TABLE_MODULE=\"$(TABLE_INCLUDE)\" Classes.c

ActionMgr.o: ActionMgr.c $(HEADER)
	$(CC) $(CFLAGS) $(INCLUDE) ActionMgr.c

Convenience.o: Convenience.c $(HEADER)
	$(CC) $(CFLAGS) $(INCLUDE) Convenience.c

Traversal.o: Traversal.c $(HEADER)
	$(CC) $(CFLAGS) $(INCLUDE) Traversal.c

# The following hack is necessary on an RS/6000. Complain to IBM.
foo.c: EnumSetGet.c $(HEADER)
	$(CC) $(CFLAGS) $(INCLUDE) -E EnumSetGet.c >foo.c

EnumSetGet.o: $(ENUMSETGET).c $(HEADER)
	$(CC) $(CFLAGS) $(INCLUDE) $(ENUMSETGET).c
	if test -f foo.o; then /bin/mv foo.o EnumSetGet.o; fi
	/bin/rm -f foo.c

DisplayMgr.o: DisplayMgr.c $(HEADER)
	$(CC) $(CFLAGS) $(INCLUDE) DisplayMgr.c

Widgets.o: Widgets.c $(HEADER)
	$(CC) $(CFLAGS) $(INCLUDE) Widgets.c

Resource.o: Resource.c $(HEADER)
	$(CC) $(CFLAGS) $(INCLUDE) Resource.c

Callbacks.o: Callbacks.c $(HEADER)
	$(CC) $(CFLAGS) $(INCLUDE) Callbacks.c

GeometryMgr.o: GeometryMgr.c $(HEADER)
	$(CC) $(CFLAGS) $(INCLUDE) GeometryMgr.c

CallbackMgr.o: CallbackMgr.c $(HEADER)
	$(CC) $(CFLAGS) $(INCLUDE) CallbackMgr.c

TranslMgr.o: TranslMgr.c $(HEADER)
	$(CC) $(CFLAGS) $(INCLUDE) TranslMgr.c

WidgetMgr.o: WidgetMgr.c $(HEADER)
	$(CC) $(CFLAGS) $(INCLUDE) WidgetMgr.c

AppMgr.o: AppMgr.c $(HEADER)
	$(CC) $(CFLAGS) $(INCLUDE) AppMgr.c

Timers.o: Timers.c $(HEADER)
	$(CC) $(CFLAGS) $(INCLUDE) Timers.c

EventMgr.o: EventMgr.c $(HEADER)
	$(CC) $(CFLAGS) $(INCLUDE) EventMgr.c

Events.o: Events.c $(HEADER)
	$(CC) $(CFLAGS) $(INCLUDE) Events.c

ResourceMgr.o: ResourceMgr.c $(HEADER)
	$(CC) $(CFLAGS) $(INCLUDE) ResourceMgr.c

ResourceSet.o: ResourceSet.c $(HEADER)
	$(CC) $(CFLAGS) $(INCLUDE) ResourceSet.c

ResourceGet.o: ResourceGet.c $(HEADER)
	$(CC) $(CFLAGS) $(INCLUDE) ResourceGet.c

PopupMgr.o: PopupMgr.c $(HEADER)
	$(CC) $(CFLAGS) $(INCLUDE) PopupMgr.c

Protocol.o: Protocol.c $(HEADER)
	$(CC) $(CFLAGS) $(INCLUDE) Protocol.c

ProtocolMgr.o: ProtocolMgr.c $(HEADER)
	$(CC) $(CFLAGS) $(INCLUDE) ProtocolMgr.c

List.o: List.c $(HEADER)
	$(CC) $(CFLAGS) $(INCLUDE) List.c

Text.o: Text.c $(HEADER)
	$(CC) $(CFLAGS) $(INCLUDE) Text.c

Cursor.o: Cursor.c $(HEADER)
	$(CC) $(CFLAGS) $(INCLUDE) Cursor.c

Graph.o: Graph.c $(HEADER)
	$(CC) $(CFLAGS) $(INCLUDE) Graph.c

ShellCmd.o: ShellCmd.c $(HEADER)
	$(CC) $(CFLAGS) $(INCLUDE) ShellCmd.c

Warnings.o: Warnings.c $(HEADER)
	$(CC) $(CFLAGS) $(INCLUDE) Warnings.c

Mwm.o: Mwm.c $(HEADER)
	$(CC) $(CFLAGS) $(INCLUDE) Mwm.c

MotifMisc.o: EnumSetGet.c $(HEADER)
	$(CC) $(CFLAGS) $(INCLUDE) MotifMisc.c

Text2.o: EnumSetGet.c $(HEADER)
	$(CC) $(CFLAGS) $(INCLUDE) Text2.c

GenTable.o: GenTable.c lisp.h $(HEADER)
	$(CC) $(CFLAGS) $(INCLUDE) -DCLASS_TABLE_MODULE=\"$(TABLE_INCLUDE)\" GenTable.c

TypeTable.o: TypeTable.c $(HEADER)
	$(CC) $(CFLAGS) $(INCLUDE) TypeTable.c

RedTypeTable.o: TypeTable.c $(HEADER)
	-/bin/rm -f RedTypeTable.c
	cp TypeTable.c RedTypeTable.c
	$(CC) $(CFLAGS) $(INCLUDE) -DGEN_TABLE RedTypeTable.c
	-/bin/rm -f RedTypeTable.c

GinaView.o: GinaView.c $(HEADER)
	$(CC) $(CFLAGS) $(INCLUDE) GinaView.c

$(TABLE_INCLUDE): GenTable
	./GenTable -c

exclio.o: exclio.c io.h
	$(CC) $(CFLAGS) $(LISP_CFLAGS) $(EXCL_INCLUDE) exclio.c
	if test ! -f ../lisp/exclio.o; then ln -s ../server/exclio.o ../lisp; fi

runstatus.o: runstatus.c io.h interface.h functions.h
	$(CC) $(CFLAGS) $(LISP_CFLAGS) $(EXCL_INCLUDE) $(INCLUDE) runstatus.c
	if test ! -f ../lisp/runstatus.o; then ln -s ../server/runstatus.o ../lisp; fi

io.o: io.c io.h
	$(CC) $(CFLAGS) $(LISP_CFLAGS) io.c
	if test ! -f ../lisp/io.o; then ln -s ../server/io.o ../lisp; fi

unixio.o: unixio.c $(HEADER)
	$(CC) $(CFLAGS) $(LISP_CFLAGS) $(INCLUDE) unixio.c

unixsocket.o: unixsocket.c
	$(CC) $(CFLAGS) $(LISP_CFLAGS) $(INCLUDE) unixsocket.c -DSOCKET_PATH=$(CLM_SOCKET)
	if test ! -f ../lisp/unixsocket.o; then ln -s ../server/unixsocket.o ../lisp; fi

install: all
	-@mkdir $(CLM_LIB) $(CLM_BIN)
	cp unixsocket.o io.o $(LISP_OBJECTS) $(CLM_LIB)
	cp $(DAEMON) $(CLM_BIN)/$(DAEMON)
	cp $(DEST) $(CLM_BIN)/$(DEST)
	-@rm $(CLM_BIN)/clmd
	echo $(CLM_BIN)/$(DAEMON) '$$*' -server $(CLM_BIN)/$(DEST) -tcp $(TCP_SOCKET)'&' > $(CLM_BIN)/clmd
	chmod +x $(CLM_BIN)/clmd

echo:
	echo $(CLM_BIN)/$(DAEMON) '$$*' -server $(CLM_BIN)/$(DEST) -tcp $(TCP_SOCKET)'&' > $(CLM_BIN)/clmd

clean:
	rm -f *.o *~ core GenTable $(DEST) $(DAEMON)

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
