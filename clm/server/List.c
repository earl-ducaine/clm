static char sccsid[] = "@(#)List.c	1.7 9/8/93";

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

#include <ctype.h>
#include <stdio.h>

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <Xm/List.h>
#include <Xm/SelectioB.h>
#include <Xm/Command.h>

#include "interface.h"
#include "functions.h"

ClmCommand *ClmFlistAddItem(cmd)
ClmCommand *cmd;
{
    Widget       widget;
    char        *msg;
    XmString     str;

    if(cmd->command != ClmListAddItem || cmd->num_arg != 3)
	GenError("ClmFlistAddItem: illegal command record");

    if( msg = LookupWidget(IntArg0(cmd), &widget, NULL, NULL) )
        GenWarn(msg);

    str = XmStringCreateLtoR(StringArg1(cmd), XmSTRING_DEFAULT_CHARSET);
    XmListAddItem(widget, str, IntArg2(cmd));
    XmStringFree (str);
    if( global.forced_output )
	XmUpdateDisplay(widget);
    return(NULL);
}

ClmCommand *ClmFlistAddItemUnselected(cmd)
ClmCommand *cmd;
{
    Widget       widget;
    char        *msg;
    XmString     str;

    if(cmd->command != ClmListAddItemUnselected || cmd->num_arg != 3)
	GenError("ClmFlistAddItemUnselected: illegal command record");

    if( msg = LookupWidget(IntArg0(cmd), &widget, NULL, NULL) )
        GenWarn(msg);

    str = XmStringCreateLtoR(StringArg1(cmd), XmSTRING_DEFAULT_CHARSET);
    XmListAddItemUnselected(widget, str, IntArg2(cmd));
    XmStringFree (str);
    if( global.forced_output )
	XmUpdateDisplay(widget);
    return(NULL);
}

ClmCommand *ClmFlistDeleteItem(cmd)
ClmCommand *cmd;
{
    Widget       widget;
    XmString     str;
    char        *msg;

    if(cmd->command != ClmListDeleteItem || cmd->num_arg != 2)
	GenError("ClmFlistDeleteItem: illegal command record");

    if( msg = LookupWidget(IntArg0(cmd), &widget, NULL, NULL) )
        GenWarn(msg);

    str = XmStringCreateLtoR(StringArg1(cmd), XmSTRING_DEFAULT_CHARSET);
    XmListDeleteItem(widget, str);
    XmStringFree (str);
    if( global.forced_output )
	XmUpdateDisplay(widget);
    return(NULL);
}

ClmCommand *ClmFlistDeletePos(cmd)
ClmCommand *cmd;
{
    Widget       widget;
    char        *msg;

    if(cmd->command != ClmListDeletePos || cmd->num_arg != 2)
	GenError("ClmFlistDeletePos: illegal command record");

    if( msg = LookupWidget(IntArg0(cmd), &widget, NULL, NULL) )
        GenWarn(msg);

    XmListDeletePos(widget, IntArg1(cmd));
    if( global.forced_output )
	XmUpdateDisplay(widget);
    return(NULL);
}

ClmCommand *ClmFlistDeselectAllItems(cmd)
ClmCommand *cmd;
{
    Widget       widget;
    char        *msg;

    if(cmd->command != ClmListDeselectAllItems || cmd->num_arg != 1)
	GenError("ClmFlistDeselectAllItems: illegal command record");

    if( msg = LookupWidget(IntArg0(cmd), &widget, NULL, NULL) )
        GenWarn(msg);

    XmListDeselectAllItems(widget);
    if( global.forced_output )
	XmUpdateDisplay(widget);
    return(NULL);
}

ClmCommand *ClmFlistDeselectItem(cmd)
ClmCommand *cmd;
{
    Widget       widget;
    char        *msg;
    XmString     str;

    if(cmd->command != ClmListDeselectItem || cmd->num_arg != 2)
	GenError("ClmFlistDeselectItem: illegal command record");

    if( msg = LookupWidget(IntArg0(cmd), &widget, NULL, NULL) )
        GenWarn(msg);

    str = XmStringCreateLtoR(StringArg1(cmd), XmSTRING_DEFAULT_CHARSET);
    XmListDeselectItem(widget, str);
    XmStringFree (str);
    if( global.forced_output )
	XmUpdateDisplay(widget);
    return(NULL);
}

ClmCommand *ClmFlistDeselectPos(cmd)
ClmCommand *cmd;
{
    Widget       widget;
    char        *msg;

    if(cmd->command != ClmListDeselectPos || cmd->num_arg != 2)
	GenError("ClmFlistDeselectPos: illegal command record");

    if( msg = LookupWidget(IntArg0(cmd), &widget, NULL, NULL) )
        GenWarn(msg);

    XmListDeselectPos(widget, IntArg1(cmd));
    if( global.forced_output )
	XmUpdateDisplay(widget);
    return(NULL);
}

ClmCommand *ClmFlistSelectItem(cmd)
ClmCommand *cmd;
{
    Widget       widget;
    char        *msg;
    XmString     str;

    if(cmd->command != ClmListSelectItem || cmd->num_arg != 3)
	GenError("ClmFlistSelectItem: illegal command record");

    if( msg = LookupWidget(IntArg0(cmd), &widget, NULL, NULL) )
        GenWarn(msg);

    str = XmStringCreateLtoR(StringArg1(cmd), XmSTRING_DEFAULT_CHARSET);
    XmListSelectItem(widget, str, IntArg2(cmd));
    XmStringFree (str);
    if( global.forced_output )
	XmUpdateDisplay(widget);
    return(NULL);
}

ClmCommand *ClmFlistSelectPos(cmd)
ClmCommand *cmd;
{
    Widget       widget;
    char        *msg;

    if(cmd->command != ClmListSelectPos || cmd->num_arg != 3)
	GenError("ClmFlistSelectPos: illegal command record");

    if( msg = LookupWidget(IntArg0(cmd), &widget, NULL, NULL) )
        GenWarn(msg);

    XmListSelectPos(widget, IntArg1(cmd), IntArg2(cmd));
    if( global.forced_output )
	XmUpdateDisplay(widget);
    return(NULL);
}

ClmCommand *ClmFlistSetBottomItem(cmd)
ClmCommand *cmd;
{
    Widget       widget;
    char        *msg;
    XmString     str;

    if(cmd->command != ClmListSetBottomItem || cmd->num_arg != 2)
	GenError("ClmFlistSetBottomItem: illegal command record");

    if( msg = LookupWidget(IntArg0(cmd), &widget, NULL, NULL) )
        GenWarn(msg);

    str = XmStringCreateLtoR(StringArg1(cmd), XmSTRING_DEFAULT_CHARSET);
    XmListSetBottomItem(widget, str);
    XmStringFree (str);
    if( global.forced_output )
	XmUpdateDisplay(widget);
    return(NULL);
}

ClmCommand *ClmFlistSetBottomPos(cmd)
ClmCommand *cmd;
{
    Widget       widget;
    char        *msg;

    if(cmd->command != ClmListSetBottomPos || cmd->num_arg != 2)
	GenError("ClmFlistSetBottomPos: illegal command record");

    if( msg = LookupWidget(IntArg0(cmd), &widget, NULL, NULL) )
        GenWarn(msg);

    XmListSetBottomPos(widget, IntArg1(cmd));
    if( global.forced_output )
	XmUpdateDisplay(widget);
    return(NULL);
}

ClmCommand *ClmFlistSetHorizPos(cmd)
ClmCommand *cmd;
{
    Widget       widget;
    char        *msg;

    if(cmd->command != ClmListSetHorizPos || cmd->num_arg != 2)
	GenError("ClmFlistSetHorizPos: illegal command record");

    if( msg = LookupWidget(IntArg0(cmd), &widget, NULL, NULL) )
        GenWarn(msg);

    XmListSetHorizPos(widget, IntArg1(cmd));
    if( global.forced_output )
	XmUpdateDisplay(widget);
    return(NULL);
}

ClmCommand *ClmFlistSetItem(cmd)
ClmCommand *cmd;
{
    Widget       widget;
    char        *msg;
    XmString     str;

    if(cmd->command != ClmListSetItem || cmd->num_arg != 2)
	GenError("ClmFlistSetItem: illegal command record");

    if( msg = LookupWidget(IntArg0(cmd), &widget, NULL, NULL) )
        GenWarn(msg);

    str = XmStringCreateLtoR(StringArg1(cmd), XmSTRING_DEFAULT_CHARSET);
    XmListSetItem(widget, str);
    XmStringFree (str);
    if( global.forced_output )
	XmUpdateDisplay(widget);
    return(NULL);
}

ClmCommand *ClmFlistSetPos(cmd)
ClmCommand *cmd;
{
    Widget       widget;
    char        *msg;

    if(cmd->command != ClmListSetPos || cmd->num_arg != 2)
	GenError("ClmFlistSetPos: illegal command record");

    if( msg = LookupWidget(IntArg0(cmd), &widget, NULL, NULL) )
        GenWarn(msg);

    XmListSetPos(widget, IntArg1(cmd));
    if( global.forced_output )
	XmUpdateDisplay(widget);
    return(NULL);
}

ClmCommand *ClmFlistSetItems(cmd)
ClmCommand *cmd;
{
    Widget       widget;
    char        *msg;
    Arg          arg[2];
    int          i, j;
    XmString    *str_ptr;

    if(cmd->command != ClmListSetItems || cmd->num_arg < 1 )
	GenError("list-set-items: illegal command record");

    if( msg = LookupWidget(IntArg0(cmd), &widget, NULL, NULL) )
	GenWarn(msg);

    if( cmd->num_arg > 1 ) {
        str_ptr = (XmString *)ClmMalloc((cmd->num_arg-1)*sizeof(XmString));
    } else {
	str_ptr = NULL;
    }

    for(i=1, j=0; i<cmd->num_arg; ++i) {
	if( cmd->args[i].arg_type != ClmArgString &&
	    cmd->args[i].arg_type != ClmArgSymbol ) {
	    continue;
	}
	str_ptr[j] = 
	  XmStringCreateLtoR(StringArgn(cmd,i), XmSTRING_DEFAULT_CHARSET);
	j++;
    }

    if( XtClass(widget) == (WidgetClass)xmListWidgetClass ) {
	XtSetArg(arg[0], XmNitems, str_ptr);
	XtSetArg(arg[1], XmNitemCount, j);
	XtSetValues(widget, arg, 2);
    }
    else {
	if( XtClass(widget) == (WidgetClass)xmSelectionBoxWidgetClass ) {
	    XtSetArg(arg[0], XmNlistItems, str_ptr);
	    XtSetArg(arg[1], XmNlistItemCount, j);
	    XtSetValues(widget, arg, 2);
	}
	else {
	    if( XtClass(widget) == (WidgetClass)xmCommandWidgetClass ) {
		XtSetArg(arg[0], XmNhistoryItems, str_ptr);
		XtSetArg(arg[1], XmNhistoryItemCount, j);
		XtSetValues(widget, arg, 2);
	    }
	}
    }
    for(i=0; i<j; i++)
        XmStringFree (str_ptr[i]);
    if (str_ptr != NULL)
        XtFree (str_ptr);
    if( global.forced_output )
	XmUpdateDisplay(widget);
    return(NULL);
}

static ClmCommand GIC = {ClmReturnValues, -1, 0, NULL};

ClmCommand *ClmFlistGetItems(cmd)
ClmCommand *cmd;
{
             Widget       widget;
             char        *msg;
	     Arg          arg[2];
    register int          i;
             XmString    *str_ptr;
	     int          item_count;
             int          all;

    /* 1. Widget 2. Boolean (all or selected items) &rest items */

    if(cmd->command != ClmListGetItems || cmd->num_arg < 2 )
	GenError("list-get-items: illegal command record");

    if( msg = LookupWidget(IntArg0(cmd), &widget, NULL, NULL) )
	GenWarn(msg);

    all = IntArg1(cmd) ? 1 : 0;

    if( XtClass(widget) == (WidgetClass)xmListWidgetClass ) {
	XtSetArg(arg[0], all ? XmNitems : XmNselectedItems, &str_ptr);
	XtSetArg(arg[1], all ? XmNitemCount : XmNselectedItemCount,&item_count);
	XtGetValues(widget, arg, 2);
    }
    else {
	if( XtClass(widget) == (WidgetClass)xmSelectionBoxWidgetClass ) {
	    if( all ) {
		XtSetArg(arg[0], XmNlistItems, &str_ptr);
		XtSetArg(arg[1], XmNlistItemCount, &item_count);
	        XtGetValues(widget, arg, 2);
	    }
	    else {
		XtSetArg(arg[0], XmNtextString, &str_ptr);
		XtGetValues(widget, arg, 1);
		item_count = 1;
	    }
	}
	else {
	    if( XtClass(widget) == (WidgetClass)xmCommandWidgetClass ) {
		XtSetArg(arg[0], XmNhistoryItems, &str_ptr);
		XtSetArg(arg[1], XmNhistoryItemCount, &item_count);
		XtGetValues(widget, arg, 2);
	    }
	}
    }

    GIC.num_arg = item_count;
    GIC.args    = (ClmArg *)ClmMalloc(item_count * sizeof(ClmArg));
    ClmErrorTest();

    for(i=0;  i < item_count; ++i) {
	GIC.args[i].arg_type = ClmArgString;
	XmStringGetLtoR(str_ptr[i], "", &GIC.args[i].v.string_value);
    }
    return(&GIC);
}
