static char sccsid[] = "@(#)Callbacks.c	1.7 9/8/93";

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
#include <Xm/Xm.h>
#if XmREVISION >= 2
#include <Xm/Protocols.h>
#else
#include <X11/Protocols.h>
#endif
#include <Xm/ArrowB.h>
#include <Xm/Command.h>
#include <Xm/DrawingA.h>
#include <Xm/DrawnB.h>
#include <Xm/FileSB.h>
#include <Xm/List.h>
#include <Xm/Scale.h>
#include <Xm/ScrollBar.h>
#include <Xm/SelectioB.h>
#include <Xm/Text.h>
#if XmREVISION != 0
#include <Xm/TextF.h>
#endif
#include <Xm/ToggleB.h>

#include <Xm/PushB.h>

#include "interface.h"
#include "functions.h"

typedef struct ClmCBI ClmCallbackInfo;

struct ClmCBI {
    char            *external_name;        /* LISP name of callback list */
    char            *internal_name;        /* Toolkit name of callback list */
    int              (*convert_to_lisp)(); /* converts call-data to lisp-rep */
    char            *(*update_call_data)();
    WidgetID         widget_id;            /* name of widget */
    Widget           widget;               /* pointer to widget */
    ClmCallbackInfo *next, *pre;           /* chain pointers */
    Boolean          in_use, destroyed;
};

static ClmCallbackInfo CallbackList = { NULL, NULL, NULL, NULL, 0, NULL, 
					&CallbackList, &CallbackList,
                                        False, False };

typedef struct {
    char *external_name;             /* LISP name of callback list */
    char *internal_name;             /* Toolkit name of callback list */
    int   (*convert_to_lisp)();      /* converts call-data to lisp-rep */
    char *(*update_call_data)();   /* processes result of callback function */
} ClmCallbackConverter;

#define MAX_HANDLER_ARGS 100

static int CBdecrement(), CBincrement(), CBdrag(), CBtoBottom(), CBtoTop(),
	   CBpageDecrement(), CBpageIncrement(), CBexpose(), CBinput(),
	   CBarm(), CBdisarm(), CBapply(), CBok(), CBcommand(),
	   CBdrawing(), CBactivate(), CBmotion(), CBmodify(), CBlosingFocus(),
	   CBvalueChanged(), CBnil(), CBsel_multi(), CBsel_ext(), CBsel_one();

static char *CBRdoit();

static ClmCallbackConverter ConverterTable[] = {
    { "ACTIVATE",           XmNactivateCallback,      CBactivate},
    { "ARM",                XmNarmCallback,           CBarm},
    { "DISARM",             XmNdisarmCallback,        CBdisarm},
    { "POPUP",              XtNpopupCallback,         CBnil},
    { "POPDOWN",            XtNpopdownCallback,       CBnil},
    { "HELP",               XmNhelpCallback,          CBnil},
    { "DECREMENT",          XmNdecrementCallback,     CBdecrement},
    { "DRAG",               XmNdragCallback,          CBdrag},
    { "INCREMENT",          XmNincrementCallback,     CBincrement},
    { "PAGE-DECREMENT",     XmNpageDecrementCallback, CBpageDecrement},
    { "PAGE-INCREMENT",     XmNpageIncrementCallback, CBpageIncrement},
    { "TO-BOTTOM",          XmNtoBottomCallback,      CBtoBottom},
    { "TO-TOP",             XmNtoTopCallback,         CBtoTop},
    { "FOCUS",              XmNfocusCallback,         CBnil},
    { "LOSING-FOCUS",       XmNlosingFocusCallback,   CBlosingFocus},
    { "MODIFY-VERIFY",      XmNmodifyVerifyCallback,  CBmodify, CBRdoit},
    { "MOTION-VERIFY",      XmNmotionVerifyCallback,  CBmotion, CBRdoit},
    { "VALUE-CHANGED",      XmNvalueChangedCallback,  CBvalueChanged},
    { "NO-MATCH",           XmNnoMatchCallback,       CBnil},
    { "CANCEL",             XmNcancelCallback,        CBnil},
    { "APPLY",              XmNapplyCallback,         CBok},
    { "OK",                 XmNokCallback,            CBok},
    { "BROWSE-SELECTION",   XmNbrowseSelectionCallback,   CBsel_one},
    { "SINGLE-SELECTION",   XmNsingleSelectionCallback,   CBsel_one},
    { "DEFAULT-ACTION",     XmNdefaultActionCallback,     CBsel_one},
    { "EXTENDED-SELECTION", XmNextendedSelectionCallback, CBsel_ext},
    { "MULTIPLE-SELECTION", XmNmultipleSelectionCallback, CBsel_multi},
    { "ENTRY",              XmNentryCallback,             CBnil},
    { "MAP",                XmNmapCallback,               CBnil},
    { "UNMAP",              XmNunmapCallback,             CBnil},
    { "CASCADING",          XmNcascadingCallback,         CBnil},
    { "COMMAND-CHANGED",    XmNcommandChangedCallback,    CBcommand},
    { "COMMAND-ENTERED",    XmNcommandEnteredCallback,    CBcommand},
    { "EXPOSE",             XmNexposeCallback,            CBexpose},
    { "INPUT",              XmNinputCallback,             CBinput},
    { "RESIZE",             XmNresizeCallback,            CBdrawing},
    { "DESTROY",            XmNdestroyCallback,           CBnil},
#if XmREVISION != 0
    { "GAIN-PRIMARY",       XmNgainPrimaryCallback,      CBnil},
    { "LOSE-PRIMARY",       XmNlosePrimaryCallback,      CBnil},
#endif
    { NULL,                          NULL,                     NULL},
};

static char *CBRdoit(widget, call_data, cmd)
Widget widget;
caddr_t call_data;
ClmCommand *cmd;
{
    XmTextVerifyCallbackStruct *cbs = (XmTextVerifyCallbackStruct *)call_data;

    if( cmd->num_arg == 0 )
	cbs->doit = False;
    else
	cbs->doit = True;
}

static int CBmotion(widget,call_data, args, num_arg)
Widget  widget;
caddr_t call_data;
ClmArg *args;
int     num_arg;
{
    XmTextVerifyCallbackStruct *cbs = (XmTextVerifyCallbackStruct *)call_data;

    args[num_arg].arg_type = ClmArgInteger;
    args[num_arg].v.int_value = cbs->currInsert;
    num_arg++;
    args[num_arg].arg_type = ClmArgInteger;
    args[num_arg].v.int_value = cbs->newInsert;
    num_arg++;
    return(num_arg);
}

static int CBlosingFocus(widget,call_data, args, num_arg)
Widget  widget;
caddr_t call_data;
ClmArg *args;
int     num_arg;
{
    XmTextVerifyCallbackStruct *cbs = (XmTextVerifyCallbackStruct *)call_data;

    args[num_arg].arg_type = ClmArgInteger;
    args[num_arg].v.int_value = cbs->currInsert;
    num_arg++;
    args[num_arg].arg_type = ClmArgInteger;
    args[num_arg].v.int_value = cbs->newInsert;
    num_arg++;
    return(num_arg);
}

static int CBmodify(widget,call_data, args, num_arg)
Widget  widget;
caddr_t call_data;
ClmArg *args;
int     num_arg;
{
    XmTextVerifyCallbackStruct *cbs = (XmTextVerifyCallbackStruct *)call_data;

    args[num_arg].arg_type = ClmArgInteger;
    args[num_arg].v.int_value = cbs->currInsert;
    num_arg++;
    args[num_arg].arg_type = ClmArgInteger;
    args[num_arg].v.int_value = cbs->newInsert;
    num_arg++;
    args[num_arg].arg_type = ClmArgInteger;
    args[num_arg].v.int_value = cbs->startPos;
    num_arg++;
    args[num_arg].arg_type = ClmArgInteger;
    args[num_arg].v.int_value = cbs->endPos;
    num_arg++;
    args[num_arg].arg_type = ClmArgString;
    args[num_arg].v.string_value = (cbs->text ? 
				   (cbs->text->ptr ? cbs->text->ptr : "") : "");
    num_arg++;
    args[num_arg].arg_type = ClmArgInteger;
    args[num_arg].v.int_value = (cbs->text ? cbs->text->length : 0 );
    num_arg++;
    if( cbs->text && cbs->text->ptr  )
	cbs->text->ptr[cbs->text->length] = '\0';
    return(num_arg);
}

static int CBactivate(widget,call_data, args, num_arg)
Widget  widget;
caddr_t call_data;
ClmArg *args;
int     num_arg;
{
    if( XtClass(widget) == (WidgetClass)xmPushButtonWidgetClass ) {

	/*XmPushButtonCallbackStruct *cbs = 
	  (XmPushButtonCallbackStruct *)call_data;

	args[num_arg].arg_type = ClmArgInteger;
	args[num_arg].v.int_value = cbs->click_count;
	num_arg++; */
        return(num_arg);
    }

    if(XtClass(widget) == (WidgetClass)xmTextWidgetClass) {

	Arg   xt_args[1];
	int   edit_mode;
	char *value;

	XtSetArg(xt_args[0], XmNeditMode, &edit_mode);
	XtGetValues(widget, xt_args, 1);
	if( edit_mode == XmSINGLE_LINE_EDIT ) {
	    XtSetArg(xt_args[0], XmNvalue, &value);
	    XtGetValues(widget, xt_args, 1);
	}
	else {
	    value = "";
	}
	args[num_arg].arg_type = ClmArgString;
	args[num_arg].v.string_value = value;
	num_arg++;
	return(num_arg);
    }

#if XmREVISION != 0
    if( XtClass(widget) == (WidgetClass)xmTextFieldWidgetClass ) {

	Arg   xt_args[1];
	char *value;

	XtSetArg(xt_args[0], XmNvalue, &value);
	XtGetValues(widget, xt_args, 1);
	args[num_arg].arg_type = ClmArgString;
	args[num_arg].v.string_value = value;
	num_arg++;
	return(num_arg);
    }
#endif

    if( XtClass(widget) == (WidgetClass)xmArrowButtonWidgetClass ) {

	/*XmArrowButtonCallbackStruct *cbs = 
	  (XmArrowButtonCallbackStruct *)call_data;

	args[num_arg].arg_type = ClmArgInteger;
	args[num_arg].v.int_value = cbs->click_count;
	num_arg++;*/
        return(num_arg);
    }

    if( XtClass(widget) == (WidgetClass)xmDrawnButtonWidgetClass ) {

	XmDrawnButtonCallbackStruct *cbs = 
	    (XmDrawnButtonCallbackStruct *)call_data;
	
	args[num_arg].arg_type = ClmArgInteger;
	args[num_arg].v.int_value = cbs->window;
	num_arg++;
	return(num_arg);
    }
    return(num_arg);
}

static int CBnil(widget,call_data, args, num_arg)
Widget  widget;
caddr_t call_data;
ClmArg *args;
int     num_arg;
{
    return(num_arg);
}

static int CBexpose(widget,call_data, args, num_arg)
Widget  widget;
caddr_t call_data;
ClmArg *args;
int     num_arg;
{
    if( XtClass(widget) == (WidgetClass)xmDrawingAreaWidgetClass ) {

	XmDrawingAreaCallbackStruct *cbs = 
	    (XmDrawingAreaCallbackStruct *)call_data;

	args[num_arg].arg_type = ClmArgInteger;
	args[num_arg].v.int_value = cbs->window;
	num_arg++;
	args[num_arg].arg_type = ClmArgInteger;
	args[num_arg].v.int_value = cbs->event->xexpose.x;
	num_arg++;
	args[num_arg].arg_type = ClmArgInteger;
	args[num_arg].v.int_value = cbs->event->xexpose.y;
	num_arg++;
	args[num_arg].arg_type = ClmArgInteger;
	args[num_arg].v.int_value = cbs->event->xexpose.width;
	num_arg++;
	args[num_arg].arg_type = ClmArgInteger;
	args[num_arg].v.int_value = cbs->event->xexpose.height;
	num_arg++;
	args[num_arg].arg_type = ClmArgInteger;
	args[num_arg].v.int_value = cbs->event->xexpose.count;
	num_arg++;
	return(num_arg);
    }
    if( XtClass(widget) == (WidgetClass)xmDrawnButtonWidgetClass ) {

	XmDrawnButtonCallbackStruct *cbs = 
	    (XmDrawnButtonCallbackStruct *)call_data;
	
	args[num_arg].arg_type = ClmArgInteger;
	args[num_arg].v.int_value = cbs->window;
	num_arg++;
	args[num_arg].arg_type = ClmArgInteger;
	args[num_arg].v.int_value = cbs->event->xexpose.x;
	num_arg++;
	args[num_arg].arg_type = ClmArgInteger;
	args[num_arg].v.int_value = cbs->event->xexpose.y;
	num_arg++;
	args[num_arg].arg_type = ClmArgInteger;
	args[num_arg].v.int_value = cbs->event->xexpose.width;
	num_arg++;
	args[num_arg].arg_type = ClmArgInteger;
	args[num_arg].v.int_value = cbs->event->xexpose.height;
	num_arg++;
	args[num_arg].arg_type = ClmArgInteger;
	args[num_arg].v.int_value = cbs->event->xexpose.count;
	num_arg++;
	return(num_arg);
    }
    return(num_arg);
}	

static int CBinput(widget,call_data, args, num_arg)
Widget  widget;
caddr_t call_data;
ClmArg *args;
int     num_arg;
{
    XmDrawingAreaCallbackStruct *cbs = (XmDrawingAreaCallbackStruct *)call_data;
    static char key_string[50];
    KeySym key_sym;

    args[num_arg].arg_type = ClmArgInteger;
    args[num_arg].v.int_value = cbs->window;
    num_arg++;
    if( cbs->event->type == KeyPress || cbs->event->type == KeyRelease ) {
	args[num_arg].arg_type = ClmArgSymbol;
	args[num_arg].v.symbol_value = ((cbs->event->type == KeyPress) ?
					"KEY-PRESS" : "KEY-RELEASE");
	num_arg++;
	args[num_arg].arg_type = ClmArgInteger;
	args[num_arg].v.int_value = cbs->event->xkey.x;
	num_arg++;
	args[num_arg].arg_type = ClmArgInteger;
	args[num_arg].v.int_value = cbs->event->xkey.y;
	num_arg++;
	args[num_arg].arg_type = ClmArgInteger;
	args[num_arg].v.int_value = cbs->event->xkey.x_root;
	num_arg++;
	args[num_arg].arg_type = ClmArgInteger;
	args[num_arg].v.int_value = cbs->event->xkey.y_root;
	num_arg++;
	args[num_arg].arg_type = ClmArgInteger;
	args[num_arg].v.int_value = cbs->event->xkey.state;
	num_arg++;
	args[num_arg].arg_type = ClmArgInteger;
	args[num_arg].v.int_value = cbs->event->xkey.keycode;
	num_arg++;
	args[num_arg].arg_type = ClmArgString;
	XLookupString((XKeyEvent *)cbs->event, key_string, 50, &key_sym, NULL);
	args[num_arg].v.string_value = XKeysymToString(key_sym);
	if( ! args[num_arg].v.string_value )
	    args[num_arg].v.string_value = "";
	num_arg++;
    } else
	if(cbs->event->type == ButtonPress ||
	   cbs->event->type == ButtonRelease) {
	    args[num_arg].arg_type = ClmArgSymbol;
	    args[num_arg].v.symbol_value = ((cbs->event->type == ButtonPress) ?
					     "BUTTON-PRESS" : "BUTTON-RELEASE");
	    num_arg++;
	    args[num_arg].arg_type = ClmArgInteger;
	    args[num_arg].v.int_value = cbs->event->xbutton.x;
	    num_arg++;
	    args[num_arg].arg_type = ClmArgInteger;
	    args[num_arg].v.int_value = cbs->event->xbutton.y;
	    num_arg++;
	    args[num_arg].arg_type = ClmArgInteger;
	    args[num_arg].v.int_value = cbs->event->xbutton.x_root;
	    num_arg++;
	    args[num_arg].arg_type = ClmArgInteger;
	    args[num_arg].v.int_value = cbs->event->xbutton.y_root;
	    num_arg++;
	    args[num_arg].arg_type = ClmArgInteger;
	    args[num_arg].v.int_value = cbs->event->xbutton.state;
	    num_arg++;
	    args[num_arg].arg_type = ClmArgInteger;
	    args[num_arg].v.int_value = cbs->event->xbutton.button;
	    num_arg++;
            args[num_arg].arg_type = ClmArgInteger;
	    args[num_arg].v.int_value =
		(int) (cbs->event->xbutton.time & 0x7fffffff);
	    num_arg++;
	}
    return(num_arg);
}	

static int CBdrawing(widget,call_data, args, num_arg)
Widget  widget;
caddr_t call_data;
ClmArg *args;
int     num_arg;
{
    if( XtClass(widget) == (WidgetClass)xmDrawingAreaWidgetClass ) {

	XmDrawingAreaCallbackStruct *cbs = 
	    (XmDrawingAreaCallbackStruct *)call_data;

	args[num_arg].arg_type = ClmArgInteger;
	args[num_arg].v.int_value = cbs->window;
	num_arg++;
	return(num_arg);
    }
    if( XtClass(widget) == (WidgetClass)xmDrawnButtonWidgetClass ) {

	XmDrawnButtonCallbackStruct *cbs = 
	    (XmDrawnButtonCallbackStruct *)call_data;
	
	args[num_arg].arg_type = ClmArgInteger;
	args[num_arg].v.int_value = cbs->window;
	num_arg++;
	return(num_arg);
    }
    return(num_arg);
}	

static int CBcommand(widget,call_data, args, num_arg)
Widget  widget;
caddr_t call_data;
ClmArg *args;
int     num_arg;
{
    XmCommandCallbackStruct *cbs = (XmCommandCallbackStruct *)call_data;

    args[num_arg].arg_type = ClmArgString;
    XmStringGetLtoR(cbs->value, XmSTRING_DEFAULT_CHARSET,
		    &args[num_arg].v.string_value);
    num_arg++;
    args[num_arg].arg_type = ClmArgInteger;
    args[num_arg].v.int_value = cbs->length;
    num_arg++;
    return(num_arg);
}

static int CBok(widget,call_data, args, num_arg)
Widget  widget;
caddr_t call_data;
ClmArg *args;
int     num_arg;
{
    if( XtClass(widget) == (WidgetClass)xmFileSelectionBoxWidgetClass ) {

	XmFileSelectionBoxCallbackStruct *cbs = 
	    (XmFileSelectionBoxCallbackStruct *)call_data;

	args[num_arg].arg_type = ClmArgString;
	XmStringGetLtoR(cbs->value, XmSTRING_DEFAULT_CHARSET, 
			&args[num_arg].v.string_value);
        num_arg++;
	args[num_arg].arg_type = ClmArgString;
	XmStringGetLtoR(cbs->mask, XmSTRING_DEFAULT_CHARSET, 
			&args[num_arg].v.string_value);
        num_arg++;
#if XmREVISION != 0
	args[num_arg].arg_type = ClmArgString;
	XmStringGetLtoR(cbs->dir, XmSTRING_DEFAULT_CHARSET, 
			&args[num_arg].v.string_value);
        num_arg++;
	args[num_arg].arg_type = ClmArgString;
	XmStringGetLtoR(cbs->pattern, XmSTRING_DEFAULT_CHARSET, 
			&args[num_arg].v.string_value);
        num_arg++;
#endif
    }
    if( XtClass(widget) == (WidgetClass)xmSelectionBoxWidgetClass ) {

	XmSelectionBoxCallbackStruct *cbs = 
	    (XmSelectionBoxCallbackStruct *)call_data;

	args[num_arg].arg_type = ClmArgString;
	XmStringGetLtoR(cbs->value, XmSTRING_DEFAULT_CHARSET, 
			&args[num_arg].v.string_value);
        num_arg++;
	return(num_arg);
    }
    return(num_arg);
}

static int CBsel_one(widget, call_data, args, num_arg)
Widget  widget;
caddr_t call_data;
ClmArg *args;
int     num_arg;
{
    XmListCallbackStruct *cbs = (XmListCallbackStruct *)call_data;

    args[num_arg].arg_type = ClmArgString;
    XmStringGetLtoR(cbs->item, XmSTRING_DEFAULT_CHARSET, 
		    &args[num_arg].v.string_value);
    num_arg++;
    args[num_arg].arg_type = ClmArgInteger;
    args[num_arg].v.int_value = cbs->item_position;
    num_arg++;
    return(num_arg);
}

static int CBsel_ext(widget, call_data, args, num_arg)
Widget  widget;
caddr_t call_data;
register ClmArg *args;
register int     num_arg;
{
    register XmListCallbackStruct *cbs = (XmListCallbackStruct *)call_data;
    register int i;

    args[num_arg].arg_type = ClmArgSymbol;
    switch(cbs->selection_type) {
	case XmINITIAL:
	    args[num_arg].v.symbol_value = "INITIAL";
	    break;
	case XmMODIFICATION:
	    args[num_arg].v.symbol_value = "MODIFICATION";
	    break;
	case XmADDITION:
	    args[num_arg].v.symbol_value = "ADDITION";
	    break;
    }
    num_arg++;
    args[num_arg].arg_type = ClmArgInteger;
    args[num_arg].v.int_value = cbs->selected_item_count;
    num_arg++;

    for(i=0; i < cbs->selected_item_count && num_arg < MAX_HANDLER_ARGS; i++){
        args[num_arg].arg_type = ClmArgString;
        XmStringGetLtoR(cbs->selected_items[i], XmSTRING_DEFAULT_CHARSET, 
		        &args[num_arg].v.string_value);
	num_arg++;
#if XmREVISION != 0
	args[num_arg].arg_type = ClmArgInteger;
	args[num_arg].v.int_value = cbs->selected_item_positions[i];
	num_arg++;
#endif
    }
    return(num_arg);
}

static int CBsel_multi(widget, call_data, args, num_arg)
Widget  widget;
caddr_t call_data;
register ClmArg *args;
register int     num_arg;
{
    register XmListCallbackStruct *cbs = (XmListCallbackStruct *)call_data;
    register int i;

    args[num_arg].arg_type = ClmArgInteger;
    args[num_arg].v.int_value = cbs->selected_item_count;
    num_arg++;

    for(i=0; i < cbs->selected_item_count && num_arg < MAX_HANDLER_ARGS; i++){
        args[num_arg].arg_type = ClmArgString;
        XmStringGetLtoR(cbs->selected_items[i], XmSTRING_DEFAULT_CHARSET, 
		        &args[num_arg].v.string_value);
	num_arg++;
#if XmREVISION != 0
	args[num_arg].arg_type = ClmArgInteger;
	args[num_arg].v.int_value = cbs->selected_item_positions[i];
	num_arg++;
#endif
    }
    return(num_arg);
}

static int CBarm(widget,call_data, args, num_arg)
Widget  widget;
caddr_t call_data;
ClmArg *args;
int     num_arg;
{
    if( XtClass(widget) == (WidgetClass)xmToggleButtonWidgetClass ) {

	XmToggleButtonCallbackStruct *cbs = 
	  (XmToggleButtonCallbackStruct *)call_data;

	args[num_arg].arg_type = ClmArgSymbol;
	args[num_arg].v.symbol_value = cbs->set ? "T" : "NIL";
	num_arg++;
        return(num_arg);
    }
    if( XtClass(widget) == (WidgetClass)xmDrawnButtonWidgetClass ) {

	XmDrawnButtonCallbackStruct *cbs = 
	    (XmDrawnButtonCallbackStruct *)call_data;
	
	args[num_arg].arg_type = ClmArgInteger;
	args[num_arg].v.int_value = cbs->window;
	num_arg++;
	return(num_arg);
    }
    return(num_arg);
}

static int CBdisarm(widget,call_data, args, num_arg)
Widget  widget;
caddr_t call_data;
ClmArg *args;
int     num_arg;
{
    if( XtClass(widget) == (WidgetClass)xmToggleButtonWidgetClass ) {

	XmToggleButtonCallbackStruct *cbs = 
	  (XmToggleButtonCallbackStruct *)call_data;

	args[num_arg].arg_type = ClmArgSymbol;
	args[num_arg].v.symbol_value = cbs->set ? "T" : "NIL";
	num_arg++;
	return(num_arg);
    }
    if( XtClass(widget) == (WidgetClass)xmDrawnButtonWidgetClass ) {

	XmDrawnButtonCallbackStruct *cbs = 
	    (XmDrawnButtonCallbackStruct *)call_data;
	
	args[num_arg].arg_type = ClmArgInteger;
	args[num_arg].v.int_value = cbs->window;
	num_arg++;
	return(num_arg);
    }
    return(num_arg);
}

static int CBdecrement(widget,call_data, args, num_arg)
Widget  widget;
caddr_t call_data;
ClmArg *args;
int     num_arg;
{
    XmScrollBarCallbackStruct *sb = (XmScrollBarCallbackStruct *)call_data;
    
    args[num_arg].arg_type = ClmArgInteger;
    args[num_arg].v.int_value = sb->value;
    num_arg++;
    return(num_arg);
}

static int CBdrag(widget,call_data, args, num_arg)
Widget  widget;
caddr_t call_data;
ClmArg *args;
int     num_arg;
{
    if( XtClass(widget) == (WidgetClass)xmScrollBarWidgetClass ) {

	XmScrollBarCallbackStruct *sb = (XmScrollBarCallbackStruct *)call_data;
	
	args[num_arg].arg_type = ClmArgInteger;
	args[num_arg].v.int_value = sb->value;
	num_arg++;
	return(num_arg);
    }

    if( XtClass(widget) == (WidgetClass)xmScaleWidgetClass ) {

	XmScaleCallbackStruct *cbs = (XmScaleCallbackStruct *)call_data;

	args[num_arg].arg_type = ClmArgInteger;
	args[num_arg].v.int_value = cbs->value;
	num_arg++;
	return(num_arg);
    }
    return(num_arg);
}

static int CBincrement(widget,call_data, args, num_arg)
Widget  widget;
caddr_t call_data;
ClmArg *args;
int     num_arg;
{
    XmScrollBarCallbackStruct *sb = (XmScrollBarCallbackStruct *)call_data;
    
    args[num_arg].arg_type = ClmArgInteger;
    args[num_arg].v.int_value = sb->value;
    num_arg++;
    return(num_arg);
}

static int CBpageDecrement(widget,call_data, args, num_arg)
Widget  widget;
caddr_t call_data;
ClmArg *args;
int     num_arg;
{
    XmScrollBarCallbackStruct *sb = (XmScrollBarCallbackStruct *)call_data;
    
    args[num_arg].arg_type = ClmArgInteger;
    args[num_arg].v.int_value = sb->value;
    num_arg++;
    return(num_arg);
}

static int CBpageIncrement(widget,call_data, args, num_arg)
Widget  widget;
caddr_t call_data;
ClmArg *args;
int     num_arg;
{
    XmScrollBarCallbackStruct *sb = (XmScrollBarCallbackStruct *)call_data;
    
    args[num_arg].arg_type = ClmArgInteger;
    args[num_arg].v.int_value = sb->value;
    num_arg++;
    return(num_arg);
}

static int CBtoBottom(widget,call_data, args, num_arg)
Widget  widget;
caddr_t call_data;
ClmArg *args;
int     num_arg;
{
    XmScrollBarCallbackStruct *sb = (XmScrollBarCallbackStruct *)call_data;
    
    args[num_arg].arg_type = ClmArgInteger;
    args[num_arg].v.int_value = sb->value;
    num_arg++;
    args[num_arg].arg_type = ClmArgInteger;
    args[num_arg].v.int_value = sb->pixel;
    num_arg++;
    return(num_arg);
}

static int CBtoTop(widget,call_data, args, num_arg)
Widget  widget;
caddr_t call_data;
ClmArg *args;
int     num_arg;
{
    XmScrollBarCallbackStruct *sb = (XmScrollBarCallbackStruct *)call_data;
    
    args[num_arg].arg_type = ClmArgInteger;
    args[num_arg].v.int_value = sb->value;
    num_arg++;
    args[num_arg].arg_type = ClmArgInteger;
    args[num_arg].v.int_value = sb->pixel;
    num_arg++;
    return(num_arg);
}

static int CBvalueChanged(widget,call_data, args, num_arg)
Widget  widget;
caddr_t call_data;
ClmArg *args;
int     num_arg;
{
    if( XtClass(widget) == (WidgetClass)xmScrollBarWidgetClass ) {

	XmScrollBarCallbackStruct *sb = (XmScrollBarCallbackStruct *)call_data;
    
	args[num_arg].arg_type = ClmArgInteger;
	args[num_arg].v.int_value = sb->value;
	num_arg++;
	return(num_arg);
    }
    if(XtClass(widget) == (WidgetClass)xmTextWidgetClass) {

	Arg   xt_args[1];
	int   edit_mode;
	char *value;

	XtSetArg(xt_args[0], XmNeditMode, &edit_mode);
	XtGetValues(widget, xt_args, 1);
	if( edit_mode == XmSINGLE_LINE_EDIT ) {
	    XtSetArg(xt_args[0], XmNvalue, &value);
	    XtGetValues(widget, xt_args, 1);
	}
	else {
	    value = "";
	}
	args[num_arg].arg_type = ClmArgString;
	args[num_arg].v.string_value = value;
	num_arg++;
	return(num_arg);
    }

#if XmREVISION != 0
    if( XtClass(widget) == (WidgetClass)xmTextFieldWidgetClass) {

	Arg   xt_args[1];
	char *value;

	XtSetArg(xt_args[0], XmNvalue, &value);
	XtGetValues(widget, xt_args, 1);
	args[num_arg].arg_type = ClmArgString;
	args[num_arg].v.string_value = value;
	num_arg++;
	return(num_arg);
    }  
#endif

    if( XtClass(widget) == (WidgetClass)xmToggleButtonWidgetClass ) {

	XmToggleButtonCallbackStruct *cbs = 
	  (XmToggleButtonCallbackStruct *)call_data;

	args[num_arg].arg_type = ClmArgSymbol;
	args[num_arg].v.symbol_value = cbs->set ? "T" : "NIL";
	num_arg++;
	return(num_arg);
    }
    if( XtClass(widget) == (WidgetClass)xmScaleWidgetClass ) {

	XmScaleCallbackStruct *cbs = (XmScaleCallbackStruct *)call_data;

	args[num_arg].arg_type = ClmArgInteger;
	args[num_arg].v.int_value = cbs->value;
	num_arg++;
	return(num_arg);
    }
    return(num_arg);
}

char *GetCallbackName(name)
char *name;
{
    ClmCallbackConverter *cvt;

    for( cvt = ConverterTable; cvt->external_name; cvt++ )
	if( ! strcmp(cvt->external_name, name) )
	    return(cvt->internal_name);
    return("NIL");
}

static ClmArg HandlerArgs[MAX_HANDLER_ARGS];
static ClmCommand HandlerCmd = {ClmEvent, -1, 0, HandlerArgs};

static void ClmCallbackHandler(widget, client_data, call_data)
Widget           widget;
ClmCallbackInfo *client_data;
caddr_t          call_data;
{
    ClmCommand *rc;

    HandlerCmd.args[0].arg_type       = ClmArgInteger;
    HandlerCmd.args[0].v.int_value    = ClmEventCallback;
    HandlerCmd.args[1].arg_type       = ClmArgInteger;
    HandlerCmd.args[1].v.int_value    = client_data->widget_id;
    HandlerCmd.args[2].arg_type       = ClmArgSymbol;
    HandlerCmd.args[2].v.symbol_value = client_data->external_name;
    HandlerCmd.num_arg = 
	(client_data->convert_to_lisp)(widget, call_data, HandlerArgs, 3);

    if(  HandlerCmd.num_arg >= MAX_HANDLER_ARGS ) {
	fprintf(stderr, "num_args (%d) > MAX_HANDLER_ARGS (%d)\n",
			HandlerCmd.num_arg, MAX_HANDLER_ARGS);
	fflush(stderr);
	abort();
    }

    if(SendCommand(global.socket, &HandlerCmd) == -1)
       abort();

    client_data->in_use = True;
    rc = ClmCallbackCommandLoop();
    if( client_data->update_call_data )
	(client_data->update_call_data)(widget,call_data,rc);
    client_data->in_use = False;

    if( client_data->destroyed ) {
	client_data->pre->next = client_data->next;
	client_data->next->pre = client_data->pre;
	free(client_data);
    }
    ClmFreeCommand(rc);
} 

char *ClmCallbackAdd(widget, widget_id, external_name)
Widget    widget;
WidgetID  widget_id;
char     *external_name;
{
           ClmCallbackInfo      *info;
           ClmCallbackConverter *cvt;
           XtCallbackStatus      status;
    static char                  err_msg[100];

    for( cvt = ConverterTable; cvt->external_name; cvt++ )
	if( ! strcmp(cvt->external_name, external_name) )
	    break;
    
    if(cvt->external_name == NULL) {
	sprintf(err_msg, "Callback list %s not found in callback-table",
			 external_name);
	return(err_msg);
    }

    status = XtHasCallbacks(widget, cvt->internal_name);
    if( status == XtCallbackNoList ) {
	sprintf(err_msg, "Callback list %s not found in widget class record", 
			 cvt->external_name);
	return(err_msg);
    }

    info = (ClmCallbackInfo *)ClmMalloc(sizeof(ClmCallbackInfo));
    
    info->widget_id       = widget_id;
    info->external_name   = cvt->external_name;
    info->internal_name   = cvt->internal_name;
    info->widget          = widget;
    info->convert_to_lisp = cvt->convert_to_lisp;
    info->update_call_data = cvt->update_call_data;
    info->in_use = info->destroyed = False;
    info->pre              = &CallbackList;
    info->next             = CallbackList.next;
    CallbackList.next->pre = info;
    CallbackList.next      = info;

    XtAddCallback(widget, info->internal_name, ClmCallbackHandler, info);

    return(NULL);
}

char *ClmCallbackRemove( widget, widget_id, external_name)
Widget    widget;
WidgetID  widget_id;
char     *external_name;
{
    ClmCallbackInfo *info;

    for( info = CallbackList.next; info != &CallbackList; info = info->next )
	if( info->widget == widget && widget_id == info->widget_id
	    && ! strcmp(external_name, info->external_name) )
	    break;
    
    if( info == &CallbackList )
	return("Callback info not found");

    XtRemoveCallback(widget, info->internal_name, ClmCallbackHandler, info);
    if( global.error_message )
        return(global.error_message);
    
    if( info->in_use )
        info->destroyed = True;
    else {
	info->pre->next = info->next;
	info->next->pre = info->pre;
	free(info);
    }
    return(NULL);
}
