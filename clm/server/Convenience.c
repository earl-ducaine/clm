static char sccsid[] = "@(#)Convenience.c	1.7 1/30/92";

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

#include <stdio.h>
#include <sys/types.h>

#include <Xm/Xm.h>
#include <X11/Shell.h>
#include <Xm/Label.h>
#include <Xm/LabelG.h>
#include <Xm/ArrowB.h>
#include <Xm/ArrowBG.h>
#include <Xm/DrawnB.h>
#include <Xm/PushB.h>
#include <Xm/PushBG.h>
#include <Xm/ToggleB.h>
#include <Xm/ToggleBG.h>
#include <Xm/CascadeB.h>
#include <Xm/CascadeBG.h>
#include <Xm/MenuShell.h>
#include <Xm/DrawingA.h>
#include <Xm/DialogS.h>
#include <Xm/BulletinB.h>
#include <Xm/Command.h>
#include <Xm/FileSB.h>
#include <Xm/Form.h>
#include <Xm/MessageB.h>
#include <Xm/SelectioB.h>
#include <Xm/ScrollBar.h>
#include <Xm/Separator.h>
#include <Xm/SeparatoG.h>
#include <Xm/Text.h>
#if XmREVISION != 0
#include <Xm/TextF.h>
#endif
#include <Xm/RowColumn.h>
#include <Xm/Scale.h>
#include <Xm/Frame.h>
#include <Xm/List.h>
#include <Xm/MainW.h>
#include <Xm/ScrolledW.h>
#include <Xm/PanedW.h>

#ifdef GRAPHWIDGET
#include <Xbab/Graph.h>
#endif

#include "interface.h"
#include "functions.h"

typedef struct _ConvFunc {
    int      position;      /* for consistency tests only */
    char    *class_name;    /* Class of widget returned by XmCreate... */
    int      class_index;   /* Position of class pointer in class table */
    Widget   (*conv_func)();/* Pointer to XmCreate... convenience function */
    Boolean  managed;       /* if widget is initially managed */
    int      args_return;   /* Number of ancestors to return 1=self,
			       2=self+parent 3=self+parent+grandparent */
    char    *implicit_class;/* for checking resources for implicit parent */
} ConvFunc;

static Widget ClmCreateApplicationShell(), ClmCreateOverrideShell(),
    ClmCreateTransientShell(), ClmCreateToplevelShell(),
    ClmCreateGinaView();

#ifdef GRAPHWIDGET
static Widget ClmCreateGraph();
#endif

/* Order of entires is critical. New Entries MUST be added at the end */
static ConvFunc convenience[] = {
{ 0, "APPLICATION-SHELL",     -1, ClmCreateApplicationShell,   False, 1, NULL},
{ 1, "ARROW-BUTTON",          -1, XmCreateArrowButton,         True,  1, NULL},
{ 2, "ARROW-BUTTON-GADGET",   -1, XmCreateArrowButtonGadget,   True,  1, NULL},
{ 3, "BULLETIN-BOARD",        -1, XmCreateBulletinBoard,       True,  1, NULL},
{ 4, "BULLETIN-BOARD",        -1, XmCreateBulletinBoardDialog, False, 2, NULL},
{ 5, "CASCADE-BUTTON",        -1, XmCreateCascadeButton,       True,  1, NULL},
{ 6, "CASCADE-BUTTON-GADGET", -1, XmCreateCascadeButtonGadget, True,  1, NULL},
{ 7, "COMMAND",               -1, XmCreateCommand,             True,  1, NULL},
{ 8, "DIALOG-SHELL",          -1, XmCreateDialogShell,         False, 1, NULL},
{ 9, "DRAWING-AREA",          -1, XmCreateDrawingArea,         True,  1, NULL},
{10, "DRAWN-BUTTON",          -1, XmCreateDrawnButton,         True,  1, NULL},
{11, "MESSAGE-BOX",           -1, XmCreateErrorDialog,         False, 2, NULL},
{12, "FILE-SELECTION-BOX",    -1, XmCreateFileSelectionBox,    True,  1, NULL},
{13, "FILE-SELECTION-BOX",    -1, XmCreateFileSelectionDialog, False, 2, 
     "DIALOG-SHELL"},
{14, "FORM",                  -1, XmCreateForm,                True,  1, NULL},
{15, "FORM",                  -1, XmCreateFormDialog,          False, 2,
     "DIALOG-SHELL"},
{16, "FRAME",                 -1, XmCreateFrame,               True,  1, NULL},
{17, "MESSAGE-BOX",           -1, XmCreateInformationDialog,   False, 2,
     "DIALOG-SHELL"},
{18, "LABEL",                 -1, XmCreateLabel,               True,  1, NULL},
{19, "LABEL-GADGET",          -1, XmCreateLabelGadget,         True,  1, NULL},
{20, "LIST",                  -1, XmCreateList,                True,  1, NULL},
{21, "MAIN-WINDOW",           -1, XmCreateMainWindow,          True,  1, NULL},
{22, "ROW-COLUMN",            -1, XmCreateMenuBar,             True,  1, NULL},
{23, "MENU-SHELL",            -1, XmCreateMenuShell,           False, 1, NULL},
{24, "MESSAGE-BOX",           -1, XmCreateMessageBox,          True,  1, NULL},
{25, "MESSAGE-BOX",           -1, XmCreateMessageDialog,       False, 2,
     "DIALOG-SHELL"},
{26, "OVERRIDE-SHELL",        -1, ClmCreateOverrideShell,      False, 1, NULL},
{27, "ROW-COLUMN",            -1, XmCreateOptionMenu,          True,  1, NULL},
{28, "PANED-WINDOW",          -1, XmCreatePanedWindow,         True,  1, NULL},
{29, "ROW-COLUMN",            -1, XmCreatePopupMenu,           False, 2, NULL},
{30, "SELECTION-BOX",         -1, XmCreatePromptDialog,        False, 2,
     "DIALOG-SHELL"},
{31, "ROW-COLUMN",            -1, XmCreatePulldownMenu,        False, 2, NULL},
{32, "PUSH-BUTTON",           -1, XmCreatePushButton,          True,  1, NULL},
{33, "PUSH-BUTTON-GADGET",    -1, XmCreatePushButtonGadget,    True,  1, NULL},
{34, "MESSAGE-BOX",           -1, XmCreateQuestionDialog,      False, 2,
     "DIALOG-SHELL"},
{35, "ROW-COLUMN",            -1, XmCreateRadioBox,            True,  1, NULL},
{36, "ROW-COLUMN",            -1, XmCreateRowColumn,           True,  1, NULL},
{37, "SCALE",                 -1, XmCreateScale,               True,  1, NULL},
{38, "SCROLL-BAR",            -1, XmCreateScrollBar,           True,  1, NULL},
{39, "LIST",                  -1, XmCreateScrolledList,        True,  1,
     "SCROLLED-WINDOW"},
{40, "TEXT",                  -1, XmCreateScrolledText,        True,  1,
     "SCROLLED-WINDOW"},
{41, "SCROLLED-WINDOW",       -1, XmCreateScrolledWindow,      True,  1, NULL},
{42, "SELECTION-BOX",         -1, XmCreateSelectionBox,        True,  1, NULL},
{43, "SELECTION-BOX",         -1, XmCreateSelectionDialog,     False, 2,
     "DIALOG-SHELL"},
{44, "SEPARATOR",             -1, XmCreateSeparator,           True,  1, NULL},
{45, "SEPARATOR-GADGET",      -1, XmCreateSeparatorGadget,     True,  1, NULL},
{46, "TEXT",                  -1, XmCreateText,                True,  1, NULL},
#if XmREVISION != 0
{47, "TEXT-FIELD",            -1, XmCreateTextField,           True,  1, NULL},
#else
{47, "TEXT-FIELD",            -1, NULL,                        True,  1, NULL},
#endif
{48, "TOGGLE-BUTTON",         -1, XmCreateToggleButton,        True,  1, NULL},
{49, "TOGGLE-BUTTON-GADGET",  -1, XmCreateToggleButtonGadget,  True,  1, NULL},
{50, "TOPLEVEL-SHELL",        -1, ClmCreateToplevelShell,      False, 1, NULL},
{51, "TRANSIENT-SHELL",       -1, ClmCreateTransientShell,     False, 1, NULL},
{52, "MESSAGE-BOX",           -1, XmCreateWarningDialog,       False, 2,
     "DIALOG-SHELL"},
#if XmREVISION != 0
{53, "ROW-COLUMN",            -1, XmCreateWorkArea,            True,  1, NULL},
#else
{53, "ROW-COLUMN",            -1, NULL,                        True,  1, NULL},
#endif
{54, "MESSAGE-BOX",           -1, XmCreateWorkingDialog,       False, 2,
     "DIALOG-SHELL"},
#ifdef GRAPHWIDGET
{55, "GRAPH",                 -1, ClmCreateGraph,              True,  1, NULL},
#else
{55, "",                      -1, NULL,                        False, 1, NULL},
#endif
{56, "DRAWING-AREA",          -1, ClmCreateGinaView,           True,  1, NULL},
};

static int table_size = sizeof(convenience)/sizeof(ConvFunc);

ClmArg     WidgetIDArgs[2];
ClmCommand WidgetIDCommand = {ClmReturnValues, -1, 0, WidgetIDArgs};

ClmCommand *ClmFconvenience(cmd)
ClmCommand *cmd;
{
    Widget          widget;
    char           *msg;
    register int    class_id, conv_id;
    int             ret_args;
    Widget          parent;
    Arg            *arglist;
    WidgetID        ret_id, parent_wid;
    extern WidgetID EncapsulateWidget();
    register        ConvFunc *cf;
    Boolean         managed;
    int             parent_class_id = -1, implicit_class_id = -1;

    /*   Call Motif convenience functions which are of form
     *   XmCreate....(parent,name,args,nargs)
     *   Format of a command record :
     *   0. Integer indicating the widget class (index for 'convenience' array)
     *   1. Parent widget 
     *   2. widget name
     *   4..n Arguments
     *   All widgets except dialog boxes are automatically managed.
     */

    if(cmd->command != ClmConvenience || cmd->num_arg < 3 ||
       (cmd->num_arg-3) % 2 != 0 ||
       (conv_id = IntArg0(cmd)) >= table_size || conv_id < 0 )
	GenError("ClmConvenience: illegal command record");

    cf = convenience+conv_id;
    
    if(cf->position != conv_id)
	GenError("Corrupted Widget Table");
    
    if( (class_id = cf->class_index) == -1 ) 
        if( (cf->class_index = class_id =
	     ClassNameToIndex(cf->class_name, &msg) ) == -1 )
	    GenError(msg);

    if (cf->implicit_class != NULL)
        if ((implicit_class_id = ClassNameToIndex(cf->implicit_class, &msg))
                == -1)
           GenError(msg);

    if((conv_id > 0) &&
       (msg = LookupWidget(IntArg1(cmd), &parent, &parent_class_id, NULL)))
	GenError(msg);

    managed = cf->managed;
    if( (arglist = ClmMakeArgList(class_id, parent_class_id, implicit_class_id,
				  cmd->args+3, cmd->num_arg-3,
				  &msg, &ret_args, &managed )) == NULL)
	GenError(msg);

    ClmErrorTest();
    widget = (cf->conv_func) (parent, StringArg2(cmd), arglist, ret_args );
    ClmErrorTest();
    if(managed)
	XtManageChild(widget);
    ClmErrorTest();

    ret_id = EncapsulateWidget (widget, EW_GENERATE_NEW_ID, class_id,
				EW_COMPUTE_CLASS_ID, &msg);
    if (ret_id < 0) {
	XtDestroyWidget (widget);
	GenError (msg);
    }

    if( conv_id == 0 ) {
	if( global.app_shell_id != -1 ) {
	    XtDestroyWidget (widget);
	    GenError("Application shell already created.");
	}
	global.app_shell_ptr = widget;
	global.app_shell_id = ret_id;
	global.no_active_app = FALSE;
    }

    if( global.forced_output )
	XmUpdateDisplay(widget);
    
    WidgetIDCommand.num_arg = cf->args_return;
    WidgetIDArgs[0].arg_type = ClmArgInteger;
    WidgetIDArgs[0].v.int_value = ret_id;
    if( cf->args_return == 2 ) {
	parent_wid = EncapsulateWidget(XtParent(widget), 
				       EW_GENERATE_ID_IF_NECESSARY,
				       EW_COMPUTE_CLASS_ID,
				       EW_COMPUTE_CLASS_ID, &msg);
	if( parent_wid < 0 ) {
	    XtDestroyWidget (widget);
	    GenError(msg);
	}
	WidgetIDArgs[1].arg_type = ClmArgInteger;
	WidgetIDArgs[1].v.int_value = parent_wid;
    }
    return(&WidgetIDCommand);
}

static Widget ClmCreateApplicationShell(parent, name, args, n_args)
Widget parent;
char *name;
Arg *args;
int n_args;
{
    return(XtAppCreateShell(name, global.app_class,
			    applicationShellWidgetClass, global.display,
			    args, n_args));
}

static Widget ClmCreateOverrideShell(parent, name, args, n_args)
Widget parent;
char *name;
Arg *args;
int n_args;
{
    return(XtCreatePopupShell(name, overrideShellWidgetClass, parent,
			      args, n_args));
}

static Widget ClmCreateTransientShell(parent, name, args, n_args)
Widget parent;
char *name;
Arg *args;
int n_args;
{
    return(XtCreatePopupShell(name, transientShellWidgetClass, parent,
			      args, n_args));
}

static Widget ClmCreateToplevelShell(parent, name, args, n_args)
Widget parent;
char *name;
Arg *args;
int n_args;
{
    return(XtCreatePopupShell(name, topLevelShellWidgetClass, parent,
			      args, n_args));
}

static Widget ClmCreateGinaView(parent, name, args, n_args)
Widget parent;
char *name;
Arg *args;
int n_args;
{
    Widget widget;

    widget = XmCreateDrawingArea(parent, name, args, n_args);
    InitializeGinaView(widget);
    return(widget);
}

#ifdef GRAPHWIDGET
static Widget ClmCreateGraph(parent, name, args, n_args)
Widget parent;
char *name;
Arg *args;
int n_args;
{
    return(XtCreateWidget(name, xmGraphWidgetClass, parent, args, n_args));
}
#endif GRAPHWIDGET

