static char sccsid[] = "@(#)Text.c	1.6 1/28/92";

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

#include <Xm/Xm.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <Xm/Text.h>
#if XmREVISION != 0
#include <Xm/TextF.h>
#endif

#if XmREVISION == 0
#include <Xm/TextP.h>
#include <Xm/ScrollBarP.h>
#include <Xm/ScrolledWP.h>
#endif

#include "interface.h"
#include "functions.h"

#if XmREVISION != 0
ClmCommand *ClmFtextInsert(cmd)
ClmCommand *cmd;
{
    Widget            widget;
    XmTextPosition    where;
    char             *msg;
    register Boolean  is_text_field;
    
    if(cmd->command != ClmTextInsert || cmd->num_arg != 5)
	GenError("text-insert: illegal command record");

    if( msg = LookupWidget(IntArg0(cmd), &widget, NULL, NULL) )
	GenWarn(msg);

    is_text_field = (XtClass(widget) == xmTextFieldWidgetClass);
    
    if( IntArg1(cmd) == -1 )
	if(is_text_field)
	    where = XmTextFieldGetLastPosition(widget);
        else	
	    where = XmTextGetLastPosition(widget);
    else
	where = (XmTextPosition)IntArg1(cmd);
    
    if(is_text_field)
	XmTextFieldInsert(widget, where, StringArgn(cmd,4));
    else	
	XmTextInsert(widget, where, StringArgn(cmd,4));
    ClmErrorTest();

    if( IntArg2(cmd) > 0 )
	if(is_text_field)
	    XmTextFieldSetInsertionPosition(widget,
					    where+strlen(StringArgn(cmd,4)));
	else
	  XmTextSetInsertionPosition(widget,where+strlen(StringArgn(cmd,4)));

    if( IntArgn(cmd,3) )
	if(is_text_field) 
	    XmTextFieldShowPosition(widget, where);
	 else 
	    XmTextShowPosition(widget, where);
    if( global.forced_output )
	XmUpdateDisplay(widget);
    return(NULL);
}
#else
ClmCommand *ClmFtextInsert(cmd)
ClmCommand *cmd;
{
    Widget          widget;
    XmTextPosition  where;
    char           *msg;
    Arg args[5];
    XmScrollBarCallbackStruct cbs;
    XmScrolledWindowWidget sw;
    XmScrollBarWidget sb;
    int minimum, value, maximum, size;

    if(cmd->command != ClmTextInsert || cmd->num_arg != 5)
	GenError("text-insert: illegal command record");

    if( msg = LookupWidget(IntArg0(cmd), &widget, NULL, NULL) )
	GenWarn(msg);

    if( IntArg1(cmd) == -1 )
        where = GetSrc(widget)->data->length;
    else
	where = IntArg1(cmd);
    
    XmTextReplace(widget, where, where, StringArgn(cmd,4));
    ClmErrorTest();

    if( IntArg2(cmd) ) {
	XtSetArg(args[0], XmNcursorPosition, where+strlen(StringArgn(cmd,4)));
	XtSetValues(widget, args, 1);
    }

    if( IntArg3(cmd) ) {
        sw = NULL;
        if (XtIsSubclass (widget->core.parent,xmScrolledWindowWidgetClass))
            sw = (XmScrolledWindowWidget )widget->core.parent;
        else 
            if (XtIsSubclass (widget->core.parent->core.parent,
                              xmScrolledWindowWidgetClass))
                sw = (XmScrolledWindowWidget )widget->core.parent->core.parent;
        if(sw) {
	    if( sw->swindow.ScrollPolicy == XmAUTOMATIC ) {
		XtSetArg(args[0], XmNverticalScrollBar, &sb);
		XtGetValues(sw, args, 1);
		XtSetArg(args[0], XmNsliderSize, &size);
		XtSetArg(args[1], XmNvalue, &value);
		XtSetArg(args[2], XmNmaximum, &maximum);
		XtSetArg(args[3], XmNminimum, &minimum);
		XtGetValues(sb, args, 4);
		XtSetArg(args[0], XmNvalue, maximum-size);
		XtSetValues(sb, args, 1);
		XtSetArg(args[0], XmNvalue, &cbs.value);
		XtGetValues(sb, args, 1);
		XtCallCallbacks(sb, XmNdragCallback, &cbs);
		global.warning_message = NULL;
	    }
        }
    }
    if( global.forced_output )
	XmUpdateDisplay(widget);
    return(NULL);
}

#endif

/* old and duplicate
static ClmArg tgs_args[1];
static ClmCommand tgs_cmd = { ClmReturnValues, -1, 1, tgs_args };

ClmCommand *ClmFgetTextSelection(cmd)
ClmCommand *cmd;
{
    Widget widget;
    char *msg, *selection, *XmTextGetSelection();

    if(cmd->command != ClmGetTextSelection || cmd->num_arg != 1)
	GenError("get-text-selection: illegal command record");

    if( msg = LookupWidget(IntArg0(cmd), &widget, NULL, NULL) )
	GenWarn(msg);

    if( XtClass(widget) == (WidgetClass)xmTextWidgetClass )
	selection = XmTextGetSelection(widget);
#if XmREVISION != 0
    if( XtClass(widget) == (WidgetClass)xmTextFieldWidgetClass )
	selection = XmTextFieldGetSelection(widget);
#endif

    selection = selection ? selection : "";

    tgs_cmd.args[0].arg_type    = ClmArgString;
    tgs_cmd.args[0].v.string_value = selection;

    return(&tgs_cmd);
}
*/


/* The following functions are new in 2.1 */

ClmCommand *ClmFtextClearSelection(cmd)
ClmCommand *cmd;
{
    Widget widget;
    char *msg;

    if(cmd->command != ClmTextClearSelection || cmd->num_arg != 1)
	GenError("text-clear-selection: illegal command record");

    if( msg = LookupWidget(IntArg0(cmd), &widget, NULL, NULL) )
	GenWarn(msg);

#if XmREVISION != 0
    if( XtClass(widget) == (WidgetClass)xmTextWidgetClass )
	XmTextClearSelection(widget, XtLastTimestampProcessed(global.display));
    if( XtClass(widget) == (WidgetClass)xmTextFieldWidgetClass )
	XmTextFieldClearSelection(widget,
				  XtLastTimestampProcessed(global.display));
#endif

    if (global.forced_output)
	XmUpdateDisplay(widget);

    return(NULL);
}

ClmArg tcpArgs[1];
ClmCommand tcpCommand = {ClmReturnValues, -1, 1, tcpArgs};

ClmCommand *ClmFtextCopy(cmd)
ClmCommand *cmd;
{
    Widget widget;
    char *msg;
    Boolean res = False;

    if(cmd->command != ClmTextCopy || cmd->num_arg != 1)
	GenError("text-copy: illegal command record");

    if( msg = LookupWidget(IntArg0(cmd), &widget, NULL, NULL) )
	GenWarn(msg);

#if XmREVISION != 0
    if( XtClass(widget) == (WidgetClass)xmTextWidgetClass )
	res = XmTextCopy(widget, XtLastTimestampProcessed(global.display));
    if( XtClass(widget) == (WidgetClass)xmTextFieldWidgetClass )
	res = XmTextFieldCopy(widget, XtLastTimestampProcessed(global.display));
#endif

    tcpArgs[0].arg_type = ClmArgSymbol;
    tcpArgs[0].v.symbol_value = res ? "T" : "NIL";

    return(&tcpCommand);
}

ClmArg tctArgs[1];
ClmCommand tctCommand = {ClmReturnValues, -1, 1, tctArgs};

ClmCommand *ClmFtextCut(cmd)
ClmCommand *cmd;
{
    Widget widget;
    char *msg;
    Boolean res = False;

    if(cmd->command != ClmTextCut || cmd->num_arg != 1)
	GenError("text-cut: illegal command record");

    if( msg = LookupWidget(IntArg0(cmd), &widget, NULL, NULL) )
	GenWarn(msg);

#if XmREVISION != 0
    if( XtClass(widget) == (WidgetClass)xmTextWidgetClass )
	res = XmTextCut(widget, XtLastTimestampProcessed(global.display));
    if( XtClass(widget) == (WidgetClass)xmTextFieldWidgetClass )
	res = XmTextFieldCut(widget, XtLastTimestampProcessed(global.display));
#endif

    tctArgs[0].arg_type = ClmArgSymbol;
    tctArgs[0].v.symbol_value = res ? "T" : "NIL";

    return(&tctCommand);
}

ClmArg tptArgs[1];
ClmCommand tptCommand = {ClmReturnValues, -1, 1, tptArgs};

ClmCommand *ClmFtextPaste(cmd)
ClmCommand *cmd;
{
    Widget widget;
    char *msg;
    Boolean res = False;

    if(cmd->command != ClmTextPaste || cmd->num_arg != 1)
	GenError("text-paste: illegal command record");

    if( msg = LookupWidget(IntArg0(cmd), &widget, NULL, NULL) )
	GenWarn(msg);

#if XmREVISION != 0
    if( XtClass(widget) == (WidgetClass)xmTextWidgetClass )
	res = XmTextPaste(widget);
    if( XtClass(widget) == (WidgetClass)xmTextFieldWidgetClass )
	res = XmTextFieldPaste(widget);
#endif

    tptArgs[0].arg_type = ClmArgSymbol;
    tptArgs[0].v.symbol_value = res ? "T" : "NIL";

    return(&tptCommand);
}

ClmArg tgbArgs[1];
ClmCommand tgbCommand = {ClmReturnValues, -1, 1, tgbArgs};

ClmCommand *ClmFtextGetBaseline(cmd)
ClmCommand *cmd;
{
    Widget widget;
    char *msg;
    int res = -1;

    if(cmd->command != ClmTextGetBaseline || cmd->num_arg != 1)
	GenError("text-get-baseline: illegal command record");

    if( msg = LookupWidget(IntArg0(cmd), &widget, NULL, NULL) )
	GenWarn(msg);

#if XmREVISION != 0
    if( XtClass(widget) == (WidgetClass)xmTextWidgetClass )
	res = XmTextGetBaseline(widget);
    if( XtClass(widget) == (WidgetClass)xmTextFieldWidgetClass )
	res = XmTextFieldGetBaseline(widget);
#endif

    tgbArgs[0].arg_type = ClmArgInteger;
    tgbArgs[0].v.int_value = res;

    return(&tgbCommand);
}

ClmArg tgsArgs[1];
ClmCommand tgsCommand = {ClmReturnValues, -1, 1, tgsArgs};

ClmCommand *ClmFtextGetSelection(cmd)
ClmCommand *cmd;
{
    Widget widget;
    char *msg;
    char *res = NULL;

    if(cmd->command != ClmTextGetSelection || cmd->num_arg != 1)
	GenError("text-get-selection: illegal command record");

    if( msg = LookupWidget(IntArg0(cmd), &widget, NULL, NULL) )
	GenWarn(msg);

    if( XtClass(widget) == (WidgetClass)xmTextWidgetClass )
	res = XmTextGetSelection(widget);
#if XmREVISION != 0
    if( XtClass(widget) == (WidgetClass)xmTextFieldWidgetClass )
	res = XmTextFieldGetSelection(widget);
#endif

    if (res == NULL) res = "";
    tgsArgs[0].arg_type = ClmArgString;
    tgsArgs[0].v.string_value = res;

    return(&tgsCommand);
}

ClmArg tgpArgs[3];
ClmCommand tgpCommand = {ClmReturnValues, -1, 3, tgpArgs};

ClmCommand *ClmFtextGetSelectionPosition(cmd)
ClmCommand *cmd;
{
    Widget widget;
    char *msg;
    Boolean res;
    XmTextPosition left, right;

    if(cmd->command != ClmTextGetSelectionPosition || cmd->num_arg != 1)
	GenError("text-get-selection-position: illegal command record");

    if( msg = LookupWidget(IntArg0(cmd), &widget, NULL, NULL) )
	GenWarn(msg);

#if XmREVISION != 0
    if( XtClass(widget) == (WidgetClass)xmTextWidgetClass )
	res = XmTextGetSelectionPosition(widget, &left, &right);
    if( XtClass(widget) == (WidgetClass)xmTextFieldWidgetClass )
	res = XmTextFieldGetSelectionPosition(widget, &left, &right);
#endif

    tgpArgs[0].arg_type = ClmArgInteger;
    tgpArgs[0].v.int_value = (res ? (int)left : -1);
    tgpArgs[1].arg_type = ClmArgInteger;
    tgpArgs[1].v.int_value = (res ? (int)right : -1);

    tgpArgs[2].arg_type = ClmArgSymbol;
    tgpArgs[2].v.symbol_value = res ? "T" : "NIL";

    return(&tgpCommand);
}

ClmArg tpxArgs[3];
ClmCommand tpxCommand = {ClmReturnValues, -1, 3, tpxArgs};

ClmCommand *ClmFtextPosToXY(cmd)
ClmCommand *cmd;
{
    Widget widget;
    char *msg;
    Boolean res;
    XmTextPosition p;
    Position x, y;

    if(cmd->command != ClmTextPosToXY || cmd->num_arg != 2)
	GenError("text-pos-to-xy: illegal command record");

    if( msg = LookupWidget(IntArg0(cmd), &widget, NULL, NULL) )
	GenWarn(msg);

    p = (XmTextPosition) IntArg1(cmd);
#if XmREVISION != 0
    if( XtClass(widget) == (WidgetClass)xmTextWidgetClass )
	res = XmTextPosToXY(widget, p, &x, &y);
    if( XtClass(widget) == (WidgetClass)xmTextFieldWidgetClass )
	res = XmTextFieldPosToXY(widget, p, &x, &y);
#endif

    tpxArgs[0].arg_type = ClmArgInteger;
    tpxArgs[0].v.int_value = (res ? (int)x : -1);
    tpxArgs[1].arg_type = ClmArgInteger;
    tpxArgs[1].v.int_value = (res ? (int)y : -1);

    tpxArgs[2].arg_type = ClmArgSymbol;
    tpxArgs[2].v.symbol_value = res ? "T" : "NIL";

    return(&tpxCommand);
}

ClmArg trmArgs[1];
ClmCommand trmCommand = {ClmReturnValues, -1, 1, trmArgs};

ClmCommand *ClmFtextRemove(cmd)
ClmCommand *cmd;
{
    Widget widget;
    char *msg;
    Boolean res = False;

    if(cmd->command != ClmTextRemove || cmd->num_arg != 1)
	GenError("text-remove: illegal command record");

    if( msg = LookupWidget(IntArg0(cmd), &widget, NULL, NULL) )
	GenWarn(msg);

#if XmREVISION != 0
    if( XtClass(widget) == (WidgetClass)xmTextWidgetClass )
	res = XmTextRemove(widget);
    if( XtClass(widget) == (WidgetClass)xmTextFieldWidgetClass )
	res = XmTextFieldRemove(widget);
#endif

    trmArgs[0].arg_type = ClmArgSymbol;
    trmArgs[0].v.symbol_value = res ? "T" : "NIL";

    return(&trmCommand);
}

ClmCommand *ClmFtextSetAddMode(cmd)
ClmCommand *cmd;
{
    Widget widget;
    char *msg;

    if(cmd->command != ClmTextSetAddMode || cmd->num_arg != 2)
	GenError("text-set-add-mode: illegal command record");

    if( msg = LookupWidget(IntArg0(cmd), &widget, NULL, NULL) )
	GenWarn(msg);

#if XmREVISION != 0
    if( XtClass(widget) == (WidgetClass)xmTextWidgetClass )
	XmTextSetAddMode(widget, (IntArg1(cmd) != 0));
    if( XtClass(widget) == (WidgetClass)xmTextFieldWidgetClass )
	XmTextFieldSetAddMode(widget, (IntArg1(cmd) != 0));
#endif

    if (global.forced_output)
	XmUpdateDisplay(widget);

    return(NULL);
}

ClmCommand *ClmFtextShowPosition(cmd)
ClmCommand *cmd;
{
    Widget widget;
    char *msg;

    if(cmd->command != ClmTextShowPosition || cmd->num_arg != 2)
	GenError("text-show-position: illegal command record");

    if( msg = LookupWidget(IntArg0(cmd), &widget, NULL, NULL) )
	GenWarn(msg);

    if( XtClass(widget) == (WidgetClass)xmTextWidgetClass )
	XmTextShowPosition(widget, (XmTextPosition) IntArg1(cmd));
#if XmREVISION != 0
    if( XtClass(widget) == (WidgetClass)xmTextFieldWidgetClass )
	XmTextFieldShowPosition(widget, (XmTextPosition) IntArg1(cmd));
#endif

    if (global.forced_output)
	XmUpdateDisplay(widget);

    return(NULL);
}

ClmArg txpArgs[1];
ClmCommand txpCommand = {ClmReturnValues, -1, 1, txpArgs};

ClmCommand *ClmFtextXYToPos(cmd)
ClmCommand *cmd;
{
    Widget widget;
    char *msg;
    XmTextPosition res;
    Position x, y;

    if(cmd->command != ClmTextXYToPos || cmd->num_arg != 3)
	GenError("text-xy-to-pos: illegal command record");

    if( msg = LookupWidget(IntArg0(cmd), &widget, NULL, NULL) )
	GenWarn(msg);

    x = (Position)IntArg1(cmd);
    y = (Position)IntArg2(cmd);
#if XmREVISION != 0
    if( XtClass(widget) == (WidgetClass)xmTextWidgetClass )
	res = XmTextXYToPos(widget, x, y);
    if( XtClass(widget) == (WidgetClass)xmTextFieldWidgetClass )
	res = XmTextFieldXYToPos(widget, x, y);
#endif

    txpArgs[0].arg_type = ClmArgInteger;
    txpArgs[0].v.int_value = (int) res;

    return(&txpCommand);
}

ClmCommand *ClmFtextScroll(cmd)
ClmCommand *cmd;
{
    Widget widget;
    char *msg;

    if(cmd->command != ClmTextScroll || cmd->num_arg != 2)
	GenError("text-scroll: illegal command record");

    if( msg = LookupWidget(IntArg0(cmd), &widget, NULL, NULL) )
	GenWarn(msg);

#if XmREVISION != 0
    if( XtClass(widget) == (WidgetClass)xmTextWidgetClass )
	XmTextScroll(widget, IntArg1(cmd));
#endif

    if (global.forced_output)
	XmUpdateDisplay(widget);

    return(NULL);
}

