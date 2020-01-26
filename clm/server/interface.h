/* "@(#)interface.h	1.6 1/28/92" */

/*
 * Copyright 1989, 1990 GMD 
 *                      (German National Research Center for Computer Science)
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of GMD not be used in advertising or
 * publicity pertaining to distribution of the software without specific,
 * written prior permission.  GMD makes no representations about the
 * suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * GMD DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL GMD
 * BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION
 * OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN 
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 * Authors: Andreas Baecker (baecker@gmdzi.gmd.de)
 *          P.O. Box 1316
 *          D-5205 Sankt Augustin 1
 */

#include "opcodes.h"
#include "io.h"

/*---- This is from ET++  !!!! */

#if defined(__GNUG__) || defined(__STDC__)
#define ANSICPP
#endif
#ifdef ANSICPP
#   define _NAME1_(name) name
#   define _NAME2_(name1,name2) name1##name2
#   define _NAME3_(name1,name2,name3) name1##name2##name3
#   define _QUOTE_(name) #name
#else
#   define _NAME1_(name) name
#   define _NAME2_(name1,name2) _NAME1_(name1)name2
#   define _NAME3_(name1,name2,name3) _NAME2_(name1,name2)name3
#   define _QUOTE_(name) "name"
#endif

#define MAXCLASSES 100

typedef struct _EventNode EventNode;
struct _EventNode {
    XEvent     event;
    EventNode *next;
};

typedef struct _WidgetNode WidgetNode;
struct _WidgetNode {
    WidgetID        id;
    Widget          widget;
    int             parent_class_id, class_id;
    XtTranslations  translations;
    EventNode      *exposures;
    EventNode      *motions;
    WidgetNode     *w_next, *w_pre;
    WidgetNode     *l_next, *l_pre;
};

typedef struct {
    char * resource_name;
    char * external_name;
    int    representation;  /* Index in representation table */
} ClmResourceDesc;

typedef struct {
    char            *name;
    WidgetClass      class;
    Widget           widget;
    int              superclass_index;
    XtResourceList   resource_list;
    ClmResourceDesc *all_resources;
    int              total_resources;
    XtResourceList   constraint_list;
    ClmResourceDesc *all_constraints;
    int              total_constraints;
} ClassEntry;

typedef struct _ClmResourceType {
    char     *resource_type;
    caddr_t (*ToolkitRep)();
    void    (*ExternalRep)();
    int       arg_type;
} ClmResourceType;

extern ClmResourceType TypeTable[];
extern ClassEntry ClassTable[MAXCLASSES];

typedef struct _ClmGlobal {
    int           socket;
    XtInputId     input_id;
    char         *error_message;
    char         *warning_message;
    Boolean       confirmed;
    Boolean       closed;
    Boolean       terminated;
    ClmArg        ErrorArgs[2];
    ClmCommand    ErrorMessage;
    Display      *display;
    XtAppContext  app_context;
    char         *app_class;
    int           must_confirm_destroy;
    WidgetID      app_shell_id;
    Widget        app_shell_ptr;
    Boolean       forced_output;
    int           no_active_app;
    int           main_loop_level;
    XtActionsRec  OneAction[1];
} ClmGlobal;

#ifndef CLM_MAIN_PROGRAM
extern ClmGlobal global;
#endif

#define GenError(msg) {do_error(msg); return(NULL);}

#define GenWarn(msg) {do_warn(msg); return(NULL);}

#define ClmErrorTest() { if( global.error_message ) \
			     GenError(global.error_message); }
