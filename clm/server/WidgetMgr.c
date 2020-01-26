static char sccsid[] = "@(#)WidgetMgr.c	1.7 1/30/92";

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

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>

#include "interface.h"
#include "functions.h"

ClmCommand *ClmFcreate(cmd) 
ClmCommand *cmd;
{
    /* Arguments:  0. name 1. class 2. parent 3. wid 4. managed 5-n: args*/

    char     *msg;
    int       class_id, parent_class_id, ret_args;
    Widget    widget;
    Widget    parent;
    Arg      *arglist;
    WidgetID  ret_id;
    Bool      gina_view;
    char     *ClassName;

    if( cmd->command != ClmCreate || cmd->num_arg < 5 || (cmd->num_arg-5) % 2 )
	GenError("ClmFcreate: Invalid command record");

    gina_view = strcmp(SymbolArg1(cmd), "GINA-VIEW") ? 0 : 1;
    ClassName = gina_view ? "DRAWING-AREA" : SymbolArg1(cmd);

    if( (class_id = ClassNameToIndex(ClassName, &msg)) == (-1))
	GenError(msg);
    
    if(msg = LookupWidget(IntArg2(cmd), &parent, &parent_class_id, NULL))
	GenError(msg);
    
    if( (arglist = ClmMakeArgList(class_id, parent_class_id, -1, cmd->args+5,
				  cmd->num_arg-5, &msg, &ret_args, NULL)) == NULL)
	GenError(msg);

    widget = XtCreateWidget(StringArg0(cmd), ClassTable[class_id].class,
			    parent, arglist, ret_args );
    if( IntArgn(cmd,4) )
	XtManageChild(widget);
    ClmErrorTest();
    
    if( gina_view )
        InitializeGinaView(widget);

    ret_id = EncapsulateWidget (widget, IntArg3(cmd), class_id,
				EW_COMPUTE_CLASS_ID, &msg);
    if (ret_id < 0) {
	XtDestroyWidget (widget);
	GenError (msg);
    }

    if( global.forced_output )
	XmUpdateDisplay(widget);
    return(NULL);
}

ClmCommand *ClmFrealize(cmd)
ClmCommand *cmd;
{
    Widget       widget;
    char        *msg;

    if(cmd->command != ClmRealize || cmd->num_arg != 1)
        GenError("ClmRealize: Illegal command record");
    
    if( msg = LookupWidget(IntArg0(cmd), &widget, NULL, NULL) )
	GenWarn(msg);

    XtRealizeWidget(widget);
    if( global.forced_output )
	XmUpdateDisplay(widget);
    return(NULL);
}

ClmCommand *ClmFsetManaged(cmd)
ClmCommand *cmd;
{
    int          i;
    Widget       widget;
    char        *msg;

    if(cmd->command != ClmSetManaged || cmd->num_arg < 1 )
	GenError("ClmSetManaged: illegal command record");

    for( i=1; i<cmd->num_arg; ++i ) {
	if( msg = LookupWidget(IntArgn(cmd,i), &widget, NULL, NULL )) 
	    GenWarn(msg);
	if( IntArg0(cmd) )
	    XtManageChild(widget);
	else
	    XtUnmanageChild(widget);
        ClmErrorTest();
    }
    if( global.forced_output )
	XmUpdateDisplay(widget);
    return(NULL);
}

ClmCommand *ClmFsetMap(cmd)
ClmCommand *cmd;
{
    int          i;
    Widget       widget;
    char        *msg;

    if(cmd->command != ClmSetMap || cmd->num_arg < 1 )
	GenError("ClmSetMap: illegal command record");

    for( i=1; i<cmd->num_arg; ++i ) {
	if( msg = LookupWidget(IntArgn(cmd,i), &widget, NULL, NULL)) 
	    GenWarn(msg);
	XtSetMappedWhenManaged(widget, IntArg0(cmd));
        ClmErrorTest();
    }
    if( global.forced_output )
	XmUpdateDisplay(widget);
    return(NULL);
}
    
ClmCommand *ClmFsetSensitivity(cmd)
ClmCommand *cmd;
{
    int          i;
    Widget       widget;
    char        *msg;

    if(cmd->command != ClmSetSensitivity || cmd->num_arg < 1 )
	GenError("ClmSetSensitivity: illegal command record");

    for( i=1; i<cmd->num_arg; ++i ) {
	if( msg = LookupWidget(IntArgn(cmd,i), &widget, NULL, NULL) ) 
	    GenWarn(msg);
	XtSetSensitive(widget, IntArg0(cmd));
        ClmErrorTest();
    }
    if( global.forced_output )
	XmUpdateDisplay(widget);
    return(NULL);
}

ClmCommand *ClmFcreatePopupShell(cmd) 
ClmCommand *cmd;
{
    /* Arguments:  0. name 1. class 2. parent 3. args 4. wid */

    char     *msg;
    int       class_id, ret_args;
    Widget    widget;
    Widget    parent;
    Arg      *arglist;
    WidgetID  ret_id;

    if( cmd->command != ClmCreatePopupShell || cmd->num_arg < 4 ||
	(cmd->num_arg-4) % 2 )
	GenError("ClmFcreatePopupShell: Invalid command record");

    if( (class_id = ClassNameToIndex(SymbolArg1(cmd), &msg) ) == -1 )
	GenError(msg);
    
    if( msg = LookupWidget(IntArg2(cmd), &parent, NULL, NULL) )
	GenError(msg);
    
    if( (arglist = ClmMakeArgList(class_id, -1, -1, cmd->args+4, cmd->num_arg-4,
				  &msg, &ret_args, NULL)) == NULL)
	GenError(msg);

    widget = XtCreatePopupShell(StringArg0(cmd), ClassTable[class_id].class,
				parent, arglist, ret_args );
    ClmErrorTest();

    ret_id = EncapsulateWidget (widget, IntArg3(cmd), class_id, -1, &msg);
    if (ret_id < 0) {
	XtDestroyWidget (widget);
	GenError (msg);
    }

    if( global.forced_output )
	XmUpdateDisplay(widget);
    return(NULL);
}

ClmCommand *ClmFdestroy(cmd)
ClmCommand *cmd;
{
    Widget       widget;
    char        *msg;

    if(cmd->command != ClmDestroy || cmd->num_arg != 1 )
	GenError("ClmCommand: illegal command record");

    if( msg = LookupWidget(IntArg0(cmd), &widget, NULL, NULL) )
	GenWarn(msg);

    XtDestroyWidget(widget);
    global.must_confirm_destroy++;
    if( global.forced_output )
	XmUpdateDisplay(widget);
    return(NULL);
}

ClmArg     WindowIDArgs[1];
ClmCommand WindowIDCommand = {ClmReturnValues, -1, 1, WindowIDArgs};

ClmCommand *ClmFgetWindowID(cmd)
ClmCommand *cmd;
{
    Widget       widget;
    char        *msg;

    if(cmd->command != ClmGetWindowID || cmd->num_arg != 1 )
	GenError("ClmGetWindowID: illegal command record");

    if( msg = LookupWidget(IntArg0(cmd), &widget, NULL, NULL) )
	GenWarn(msg);

    if( ! XtIsRealized(widget) )
	GenWarn("get-window-id: widget not realized");

    XSync(XtDisplay(widget),False);
    WindowIDArgs[0].arg_type = ClmArgInteger;
    WindowIDArgs[0].v.int_value = (int)XtWindow(widget);
    return(&WindowIDCommand);
}

/*
WidgetID EncapsulateWidget(widget, class, msg_ptr)
Widget   widget;
int      class;
char   **msg_ptr;
{
    int      parent_class;
    WidgetID id;

    if( (id = FindWidget(widget)) == -1 ) {
	if( (parent_class = ClassPointerToIndex(XtClass(widget))) == -1 ) {
	    *msg_ptr = "Class of widget not found";
	    return(-1);
	}
	else {
	    id = GenWidgetID();
	    if( *msg_ptr = InsertWidget(id, widget, class, parent_class) ) {
		return(-1);
	    }
	}
    }
    return(id);
}
*/

WidgetID EncapsulateWidget(widget, widget_id, class_id,
			   parent_class_id, msg_ptr)
Widget   widget;
WidgetID widget_id;
int      class_id, parent_class_id;
char   **msg_ptr;
{
	WidgetID known_id;
	Widget parent;
	WidgetClass parent_class;

	/*
	 * widget_id must be one of the following:
	 *
	 * 1. A valid widget ID (i.e. a positive integer)
	 *
	 * This means use this widget ID for the widget.  An error is
	 * signaled if the widget is found in the table.
	 * 
	 * 2. The symbolic constant EW_GENERATE_NEW_ID
	 * 
	 * This means this routine should generate a widget ID for the
	 * widget.  An error is signaled if the widget is found in the
	 * table.
	 * 
	 * 3. The symbolic constant EW_GENERATE_ID_IF_NECESSARY
	 * 
	 * This is similar to EW_GENERATE_NEW_ID except that if the
	 * widget is found in the table the widget's ID is returned.
	 * Only if the widget isn't found is a new widget ID generate.
	 * 
	 */

	known_id = FindWidget(widget);
	if (known_id != -1)
	    if ((widget_id > 0) || (widget_id == EW_GENERATE_NEW_ID)) {
		*msg_ptr = "Widget already in widget table but should not.";
		return -1;
	    } else
		return known_id;

	/* Callers may specify EW_COMPUTE_CLASS_ID for the class_id */
	/* which means that the class ID should be computed. */
	if (class_id == EW_COMPUTE_CLASS_ID)
	    class_id = ClassPointerToIndex(XtClass(widget));

	if (parent_class_id == EW_COMPUTE_CLASS_ID) {
	    parent = XtParent(widget);
	    if (parent)
		parent_class_id = ClassPointerToIndex(XtClass(parent));
	    else
		parent_class_id = -1;
	}

        if ((widget_id == EW_GENERATE_NEW_ID) ||
	    (widget_id == EW_GENERATE_ID_IF_NECESSARY))
	    widget_id = GenWidgetID();

	*msg_ptr = InsertWidget(widget_id, widget, class_id, parent_class_id);
	if (*msg_ptr)
	    return -1;

	/* Just inserted a new widget in the table so register a */
	/* destroy callback so that we can get it out of the table */
	/* should it be destroyed. */
	RegisterDestroyCallback(widget, widget_id);

	return widget_id;
  }
 
ClmArg     ParentArgs[1];
ClmCommand ParentCommand = {ClmReturnValues, -1, 1, ParentArgs};

ClmCommand *ClmFgetParent(cmd)
ClmCommand *cmd;
{
    Widget      widget, parent;
    char       *msg;
    WidgetID    id;
    int         class, parent_class, grandparent_class;

    if(cmd->command != ClmGetParent || cmd->num_arg != 1 )
	GenError("get-parent: illegal command record");

    if( msg = LookupWidget(IntArg0(cmd), &widget, NULL, NULL) )
	GenWarn(msg);

    parent = XtParent(widget);
    id = EncapsulateWidget (parent, EW_GENERATE_ID_IF_NECESSARY,
			    EW_COMPUTE_CLASS_ID, EW_COMPUTE_CLASS_ID,
			    &msg);
    if (id < 0)
	GenError (msg);

    ParentArgs[0].arg_type    = ClmArgInteger;
    ParentArgs[0].v.int_value = (int)id;

    return(&ParentCommand);
}

ClmCommand *ClmFraiseWindow(cmd)
ClmCommand *cmd;
{
    Widget widget;
    char   *msg;
    XWindowAttributes attr;

    if(cmd->command != ClmRaiseWindow || cmd->num_arg != 1 )
	GenError("RaiseWindow: illegal command record");

    if( msg = LookupWidget(IntArg0(cmd), &widget, NULL, NULL) )
	GenWarn(msg);

    if( ! XtIsRealized(widget) )
	GenWarn("Widget must be realized before it is raised!");

    XGetWindowAttributes(global.display,XtWindow(widget),&attr);
    XMapRaised(global.display,XtWindow(widget));
    if( attr.map_state == IsViewable )
	XSetInputFocus(global.display,XtWindow(widget),RevertToParent,
		       CurrentTime);
    if( global.forced_output )
	XmUpdateDisplay(widget);
    return(NULL);
}
