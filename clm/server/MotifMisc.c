static char sccsid[] = "@(#)MotifMisc.c	1.7 9/21/93";

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
 *
 * Most of this file contributed by:
 *          Christopher Hoover (ch@csi.com)
 *
 * ClmFrunStatus by Chris Richardson (cer@franz.com)
 */

#include <stdio.h>
#include <sys/types.h>

#include <Xm/Xm.h>
#include <X11/Intrinsic.h>
#include <X11/IntrinsicP.h>
#include <X11/Shell.h>
#include <X11/ShellP.h>
#include <X11/StringDefs.h>
#include <Xm/SelectioB.h>
#include <Xm/MessageB.h>
#include <Xm/FileSB.h>
#include <Xm/RowColumn.h>
#include <Xm/ScrolledW.h>
#include <Xm/Command.h>
#include <Xm/MainW.h>
#include <Xm/CascadeB.h>
#include <Xm/CascadeBG.h>

#include "interface.h"
#include "functions.h"


/* Support for ClmF<mumble>GetChild() */

static WidgetID
GetMumbleChild(wid, get_child_function, child, w_class, msg)
WidgetID wid;
Widget (*get_child_function)();
unsigned char child;
WidgetClass w_class;
char **msg;
{
    Widget widget, child_widget;
    int widget_class_id, child_class_id;
    WidgetClass child_class;
    WidgetID child_wid;

    if (*msg = LookupWidget(wid, &widget, &widget_class_id, NULL) )
	return (-1);

    if (!XtIsSubclass(widget, w_class)) {
	*msg = "<>-get-child: Widget has wrong class";
	return (-1);
    }
    
    if (child == (unsigned char)255)
        child_widget = (get_child_function)(widget);
    else
        child_widget = (get_child_function)(widget, child);

    child_wid = EncapsulateWidget (child_widget, EW_GENERATE_ID_IF_NECESSARY,
				   EW_COMPUTE_CLASS_ID, EW_COMPUTE_CLASS_ID,
				   msg);
    return child_wid;
}

static int
GetMumbleChildWhichChild(string, msg)
char *string, **msg;
{
        *msg = NULL;
	if (!strcmp(string, "DIALOG-APPLY-BUTTON") ||
	    !strcmp(string, "APPLY-BUTTON"))	
	        return XmDIALOG_APPLY_BUTTON;
	if (!strcmp(string, "DIALOG-CANCEL-BUTTON") ||
	    !strcmp(string, "CANCEL-BUTTON"))
		return XmDIALOG_CANCEL_BUTTON;
	if (!strcmp(string, "DIALOG-DEFAULT-BUTTON") ||
	    !strcmp(string, "DEFAULT-BUTTON"))
		return XmDIALOG_DEFAULT_BUTTON;
	if (!strcmp(string, "DIALOG-HELP-BUTTON") ||
	    !strcmp(string, "HELP-BUTTON"))
		return XmDIALOG_HELP_BUTTON;
	if (!strcmp(string, "DIALOG-OK-BUTTON") ||
	    !strcmp(string, "OK-BUTTON"))
		return XmDIALOG_OK_BUTTON;
	if (!strcmp(string, "DIALOG-LIST") ||
	    !strcmp(string, "LIST"))
		return XmDIALOG_LIST;
	if (!strcmp(string, "DIALOG-HISTORY-LIST") ||
	    !strcmp(string, "HISTORY-LIST"))
		return XmDIALOG_HISTORY_LIST;
	if (!strcmp(string, "DIALOG-LIST-LABEL") ||
	    !strcmp(string, "LIST-LABEL"))
		return XmDIALOG_LIST_LABEL;
	if (!strcmp(string, "DIALOG-PROMPT-LABEL") ||
	    !strcmp(string, "PROMPT-LABEL"))
		return XmDIALOG_PROMPT_LABEL;
	if (!strcmp(string, "DIALOG-SELECTION-LABEL") ||
	    !strcmp(string, "SELECTION-LABEL"))
		return XmDIALOG_SELECTION_LABEL;
	if (!strcmp(string, "DIALOG-SEPARATOR") ||
	    !strcmp(string, "SEPARATOR"))
		return XmDIALOG_SEPARATOR;
	if (!strcmp(string, "DIALOG-TEXT") ||
	    !strcmp(string, "TEXT"))
		return XmDIALOG_TEXT;
	if (!strcmp(string, "DIALOG-VALUE-TEXT") ||
	    !strcmp(string, "VALUE-TEXT"))
	    return XmDIALOG_VALUE_TEXT;
	if (!strcmp(string, "DIALOG-COMMAND-TEXT") ||
	    !strcmp(string, "COMMAND-TEXT"))
		return XmDIALOG_COMMAND_TEXT;
	if (!strcmp(string, "DIALOG-WORK-AREA") ||
	    !strcmp(string, "WORK-AREA"))
		return XmDIALOG_WORK_AREA;
	if (!strcmp(string, "DIALOG-MESSAGE-LABEL") ||
	    !strcmp(string, "MESSAGE-LABEL"))
		return XmDIALOG_MESSAGE_LABEL;
	if (!strcmp(string, "DIALOG-SYMBOL-LABEL") ||
	    !strcmp(string, "SYMBOL-LABEL"))
		return XmDIALOG_SYMBOL_LABEL;
	if (!strcmp(string, "DIALOG-FILTER-LABEL") ||
	    !strcmp(string, "FILTER-LABEL"))
		return XmDIALOG_FILTER_LABEL;
	if (!strcmp(string, "DIALOG-FILTER-TEXT") ||
	    !strcmp(string, "FILTER-TEXT"))
		return XmDIALOG_FILTER_TEXT;
#if XmREVISION != 0
	if (!strcmp(string, "DIALOG-DIR-LIST-LABEL") || 
	    !strcmp(string, "DIR-LIST-LABEL"))
		return XmDIALOG_DIR_LIST_LABEL;
	if (!strcmp(string, "DIALOG-DIR-LIST") ||
	    !strcmp(string, "DIR-LIST"))
		return XmDIALOG_DIR_LIST;
	if (!strcmp(string, "DIALOG-FILE-LIST-LABEL") ||
	    !strcmp(string, "FILE-LIST-LABEL"))
		return XmDIALOG_FILE_LIST_LABEL;
	if (!strcmp(string, "DIALOG-FILE-LIST") ||
	    !strcmp(string, "FILE-LIST"))
		return XmDIALOG_FILE_LIST;
#endif

	*msg = "GetMumbleChildWhichChild: bogus child name";
	return 0; 
}
    

/* ClmF<mumble>GetChild() */

ClmArg     SBChildArgs[1];
ClmCommand SBChildCommand = {ClmReturnValues, -1, 1, SBChildArgs};

ClmCommand *ClmFselectionBoxGetChild(cmd)
ClmCommand *cmd;
{
    WidgetID wid, child_wid;
    unsigned char child;
    char *msg;

    if (cmd->command != ClmSelectionBoxGetChild || cmd->num_arg != 2 )
	GenError("ClmSelectionBoxGetChild: illegal command record");
    
    wid = IntArg0(cmd);
    
    child = GetMumbleChildWhichChild(SymbolArg1(cmd), &msg);
    if (msg != NULL) GenWarn(msg);
    child_wid = GetMumbleChild(wid, XmSelectionBoxGetChild, child,
			       xmSelectionBoxWidgetClass, &msg);
    if (child_wid == -1) GenWarn(msg);
    
    SBChildArgs[0].arg_type = ClmArgInteger;
    SBChildArgs[0].v.int_value = (int) child_wid;
    return &SBChildCommand;
}


ClmArg     CmdChildArgs[1];
ClmCommand CmdChildCommand = {ClmReturnValues, -1, 1, CmdChildArgs};

ClmCommand *ClmFcommandGetChild(cmd)
ClmCommand *cmd;
{
    WidgetID wid, child_wid;
    unsigned char child;
    char *msg;

    if (cmd->command != ClmCommandGetChild || cmd->num_arg != 2 )
	GenError("ClmCommandGetChild: illegal command record");
    
    wid = IntArg0(cmd);
    
    child = GetMumbleChildWhichChild(SymbolArg1(cmd), &msg);
    if (msg != NULL) GenWarn(msg);
    child_wid = GetMumbleChild(wid, XmCommandGetChild, child,
			       xmCommandWidgetClass, &msg);
    if (child_wid == -1) GenWarn(msg);
    
    CmdChildArgs[0].arg_type = ClmArgInteger;
    CmdChildArgs[0].v.int_value = (int) child_wid;
    return &CmdChildCommand;
}


ClmArg     MBChildArgs[1];
ClmCommand MBChildCommand = {ClmReturnValues, -1, 1, MBChildArgs};

ClmCommand *ClmFmessageBoxGetChild(cmd)
ClmCommand *cmd;
{
    WidgetID wid, child_wid;
    unsigned char child;
    char *msg;

    if (cmd->command != ClmMessageBoxGetChild || cmd->num_arg != 2 )
	GenError("ClmMessageBoxGetChild: illegal command record");
    
    wid = IntArg0(cmd);
    
    child = GetMumbleChildWhichChild(SymbolArg1(cmd), &msg);
    if (msg != NULL) GenWarn(msg);
    child_wid = GetMumbleChild(wid, XmMessageBoxGetChild, child,
			       xmMessageBoxWidgetClass, &msg);
    if (child_wid == -1) GenWarn(msg);
    
    MBChildArgs[0].arg_type = ClmArgInteger;
    MBChildArgs[0].v.int_value = (int) child_wid;
    return &MBChildCommand;
}


ClmArg     FSBChildArgs[1];
ClmCommand FSBChildCommand = {ClmReturnValues, -1, 1, FSBChildArgs};

ClmCommand *ClmFfileSelectionBoxGetChild(cmd)
ClmCommand *cmd;
{
    WidgetID wid, child_wid;
    unsigned char child;
    char *msg;

    if (cmd->command != ClmFileSelectionBoxGetChild || cmd->num_arg != 2 )
	GenError("ClmFileSelectionBoxGetChild: illegal command record");
    
    wid = IntArg0(cmd);
    
    child = GetMumbleChildWhichChild(SymbolArg1(cmd), &msg);
    if (msg != NULL) GenWarn(msg);
    child_wid = GetMumbleChild(wid, XmFileSelectionBoxGetChild, child,
			       xmFileSelectionBoxWidgetClass, &msg);
    if (child_wid == -1) GenWarn(msg);
    
    FSBChildArgs[0].arg_type = ClmArgInteger;
    FSBChildArgs[0].v.int_value = (int) child_wid;
    return &FSBChildCommand;
}


ClmArg     OBChildArgs[1];
ClmCommand OBChildCommand = {ClmReturnValues, -1, 1, OBChildArgs};

ClmCommand *ClmFoptionButtonGadget(cmd)
ClmCommand *cmd;
{
    WidgetID wid, child_wid;
    char *msg;

    if (cmd->command != ClmOptionButtonGadget || cmd->num_arg != 1 )
	GenError("ClmOptionButtonGadget: illegal command record");
    
    wid = IntArg0(cmd);
    
    child_wid = GetMumbleChild(wid, XmOptionButtonGadget, (unsigned char)255,
			       xmRowColumnWidgetClass, &msg);
    if (child_wid == -1) GenWarn(msg);
    
    OBChildArgs[0].arg_type = ClmArgInteger;
    OBChildArgs[0].v.int_value = (int) child_wid;
    return &OBChildCommand;
}


ClmArg     OLChildArgs[1];
ClmCommand OLChildCommand = {ClmReturnValues, -1, 1, OLChildArgs};

ClmCommand *ClmFoptionLabelGadget(cmd)
ClmCommand *cmd;
{
    WidgetID wid, child_wid;
    char *msg;

    if (cmd->command != ClmOptionLabelGadget || cmd->num_arg != 1 )
	GenError("ClmOptionLabelGadget: illegal command record");
    
    wid = IntArg0(cmd);
    
    child_wid = GetMumbleChild(wid, XmOptionLabelGadget, (unsigned char)255,
			       xmRowColumnWidgetClass, &msg);
    if (child_wid == -1) GenWarn(msg);
    
    OLChildArgs[0].arg_type = ClmArgInteger;
    OLChildArgs[0].v.int_value = (int) child_wid;
    return &OLChildCommand;
}


/* ClmF<mumble>SetAreas() */

ClmCommand *ClmFmainWindowSetAreas(cmd)
ClmCommand *cmd;
{
    Widget mainw, menu, command, hscroll, vscroll, wregion;
    
    if (cmd->command != ClmMainWindowSetAreas || cmd->num_arg != 6 )
	GenError("MainWindowSetAreas: illegal command record");

#define FROB(widget,n) 							\
    {									\
	    char   *msg;						\
	    int arg;							\
									\
	    arg = IntArgn(cmd,n);					\
	    if (arg == 0) {						\
		    widget = NULL;					\
	    } else {							\
		    if (msg = LookupWidget(arg, &widget, NULL, NULL))	\
			    GenWarn(msg);				\
	    }								\
    }

    FROB(mainw,0);
    FROB(menu,1);
    FROB(command,2);
    FROB(hscroll,3);
    FROB(vscroll,4);
    FROB(wregion,5);

#undef FROB

    if (XtIsSubclass (mainw, xmMainWindowWidgetClass))
	XmMainWindowSetAreas(mainw, menu, command, hscroll, vscroll, wregion);
    else
	GenWarn ("main-window-set-areas: wrong widget class");
    return(NULL);
}

ClmCommand *ClmFscrolledWindowSetAreas(cmd)
ClmCommand *cmd;
{
    Widget scrolledw, hscroll, vscroll, wregion;
    
    if (cmd->command != ClmScrolledWindowSetAreas || cmd->num_arg != 4 )
	GenError("ScrolledWindowSetAreas: illegal command record");

#define FROB(widget,n) 							\
    {									\
	    char   *msg;						\
	    int arg;							\
									\
	    arg = IntArgn(cmd,n);					\
	    if (arg == 0) {						\
		    widget = NULL;					\
	    } else {							\
		    if (msg = LookupWidget(arg, &widget, NULL, NULL))	\
			    GenWarn(msg);				\
	    }								\
    }

    FROB(scrolledw,0);
    FROB(hscroll,1);
    FROB(vscroll,2);
    FROB(wregion,3);

#undef FROB

    if (XtIsSubclass (scrolledw, xmScrolledWindowWidgetClass))
	XmScrolledWindowSetAreas(scrolledw, hscroll, vscroll, wregion);
    else
	GenWarn ("scrolled-window-set-areas: wrong widget class");
    
    return(NULL);
}


/* ClmFisRealized() */

ClmArg IsRealizedArgs[1];
ClmCommand IsRealizedCommand = {ClmReturnValues, -1, 1, IsRealizedArgs};

ClmCommand *ClmFisRealized(cmd)
ClmCommand *cmd;
{
	Widget widget;
	char *msg;

	if ((cmd->command != ClmIsRealized) || (cmd->num_arg != 1))
		GenError("IsRealized: illegal command record");

	if (msg = LookupWidget(IntArg0(cmd), &widget, NULL, NULL))
		GenWarn(msg);

	IsRealizedArgs[0].arg_type = ClmArgSymbol;
	if (XtIsRealized(widget))
		IsRealizedArgs[0].v.symbol_value = "T";
	else
		IsRealizedArgs[0].v.symbol_value = "NIL";

	return(&IsRealizedCommand);

}

/* ClmFisManaged() */

ClmArg IsManagedArgs[1];
ClmCommand IsManagedCommand = {ClmReturnValues, -1, 1, IsManagedArgs};

ClmCommand *ClmFisManaged(cmd)
ClmCommand *cmd;
{
	Widget widget;
	char *msg;

	if ((cmd->command != ClmIsManaged) || (cmd->num_arg != 1))
		GenError("IsManaged: illegal command record");

	if (msg = LookupWidget(IntArg0(cmd), &widget, NULL, NULL))
		GenWarn(msg);

	IsManagedArgs[0].arg_type = ClmArgSymbol;
	if (XtIsManaged(widget))
		IsManagedArgs[0].v.symbol_value = "T";
	else
		IsManagedArgs[0].v.symbol_value = "NIL";

	return(&IsManagedCommand);

}


/* ClmFgetMultiClickTime() */

ClmArg GetMultiClickTimeArgs[1];
ClmCommand GetMultiClickTimeCommand =
	{ClmReturnValues, -1, 1, GetMultiClickTimeArgs};

ClmCommand *ClmFgetMultiClickTime(cmd)
ClmCommand *cmd;
{
	Widget widget;
	char *msg;

	if ((cmd->command != ClmGetMultiClickTime) || (cmd->num_arg != 1))
		GenError("getMultiClickTime: illegal command record");

	if (msg = LookupWidget(IntArg0(cmd), &widget, NULL, NULL))
		GenWarn(msg);

	GetMultiClickTimeArgs[0].arg_type = ClmArgInteger;
	GetMultiClickTimeArgs[0].v.int_value =
#if XmREVISION != 0
		XtGetMultiClickTime(XtDisplay(widget));
#else
                0;
#endif

	return(&GetMultiClickTimeCommand);
}


/* ClmFisValidWidgetId */

ClmArg IsValidWidgetIDArgs[1];
ClmCommand IsValidWidgetIDCommand =
	{ClmReturnValues, -1, 1, IsValidWidgetIDArgs};

ClmCommand *ClmFisValidWidgetID(cmd)
ClmCommand *cmd;
{
	int id;

	if ((cmd->command != ClmIsValidWidgetID) || (cmd->num_arg != 1))
		GenError("IsValidWidgetID: illegal command record");

	id = IntArg0(cmd);

	IsValidWidgetIDArgs[0].arg_type = ClmArgSymbol;
	IsValidWidgetIDArgs[0].v.symbol_value = "NIL";

	if (id > 0) {
		Widget widget;
		char *msg;

		msg = LookupWidget(IntArg0(cmd), &widget, NULL, NULL);
#if XmREVISION != 0
		if ((msg == NULL) && (XtIsObject(widget)))
#else
		if ((msg == NULL))
#endif
			IsValidWidgetIDArgs[0].v.symbol_value = "T";
	}

	return(&IsValidWidgetIDCommand);
}


/* Widget Names and Classes */

static char *
widget_name(w)
Widget w;
{
	char *names[100];
	int name_length;
	int length;
	char *name, *name_cat;

	name_length = 0;
	for (length = 0; w != NULL; w = XtParent(w)) {
		char *s;

#if XmREVISION != 0
		s = XtName(w);
#else
		s = "widget";
#endif
		names[length] = s;
		name_length += strlen(s);

		length++;
	}
	
	/* need space for <mumble>_length characters, (length - 1) */
	/* periods, and a null byte. */
	name = name_cat = XtMalloc(name_length + (length - 1) + 1);
	
	while (1) {
		char *s;

		length--;
		
		for (s = names[length]; *s; s++)
			*name_cat++ = *s;

		if (!length) {
			*name_cat = 0;
			break;
		}

		*name_cat++ = '.';
	}

	return name;
}

static char *
widget_class(w)
Widget w;
{
	char *classes[100];
	int class_length;
	int length;
	char *class, *class_cat;

	class_length = 0;
	for (length = 0; w != NULL; w = XtParent(w)) {
		WidgetClass class;
		char *s;

		class = XtClass(w);
		if ((w->core.parent == NULL) && XtIsApplicationShell(w))
			s = ((ApplicationShellWidget) w)->application.class;
		else
			s = class->core_class.class_name;
		
		classes[length] = s;
		class_length += strlen(s);

		length++;
	}
	
	/* need space for <mumble>_length characters, (length - 1) */
	/* periods, and a null byte. */
	class = class_cat = XtMalloc(class_length + (length - 1) + 1);
	
	while (1) {
		char *s;

		length--;
		
		for (s = classes[length]; *s; s++)
			*class_cat++ = *s;

		if (!length) {
			*class_cat = 0;
			break;
		}
		
		*class_cat++ = '.';
	}

	return class;
}


ClmArg WidgetFullNameArgs[1];
ClmCommand WidgetFullNameCommand =
	{ClmReturnValues, -1, 1, WidgetFullNameArgs};

ClmCommand *ClmFwidgetFullName(cmd)
ClmCommand *cmd;
{
	static char *name;
	Widget widget;
	char *msg;

	if ((cmd->command != ClmWidgetFullName) || (cmd->num_arg != 1))
		GenError("WidgetFullName: illegal command record");

	if (msg = LookupWidget(IntArg0(cmd), &widget, NULL, NULL))
		GenWarn(msg);

	if (name)
		XtFree(name);

	name = widget_name(widget);
	
	WidgetFullNameArgs[0].arg_type = ClmArgString;
	WidgetFullNameArgs[0].v.string_value = name;

	return &WidgetFullNameCommand;
}


ClmArg WidgetFullClassArgs[1];
ClmCommand WidgetFullClassCommand =
	{ClmReturnValues, -1, 1, WidgetFullClassArgs};

ClmCommand *ClmFwidgetFullClass(cmd)
ClmCommand *cmd;
{
	static char *name;
	Widget widget;
	char *msg;

	if ((cmd->command != ClmWidgetFullClass) || (cmd->num_arg != 1))
		GenError("WidgetFullClass: illegal command record");

	if (msg = LookupWidget(IntArg0(cmd), &widget, NULL, NULL))
		GenWarn(msg);

	if (name)
		XtFree(name);

	name = widget_class(widget);
	
	WidgetFullClassArgs[0].arg_type = ClmArgString;
	WidgetFullClassArgs[0].v.string_value = name;

	return &WidgetFullClassCommand;
}


ClmCommand *ClmFrunStatus(cmd)
ClmCommand *cmd;
{
    Widget widget;
    int class_id;
    Arg args[10];
    int n = 0;
    static XmString runstring, sleepstring, gcstring, fullgcstring;
    XmString string = (XmString) NULL;
    char *msg;

    if (runstring == 0) {
	runstring = XmStringCreateLtoR("Run", XmSTRING_DEFAULT_CHARSET);
	sleepstring  = XmStringCreateLtoR("Idle", XmSTRING_DEFAULT_CHARSET);
	gcstring  = XmStringCreateLtoR("Gc", XmSTRING_DEFAULT_CHARSET);
	fullgcstring  = XmStringCreateLtoR("Full Gc", 
					   XmSTRING_DEFAULT_CHARSET);
    }

    switch (IntArg1(cmd)) {
    case 0:
	string = sleepstring;
	break;
    case 1:
	string = runstring;
	break;
    case 2:
	string = gcstring;
	break;
    case 3:
	string = fullgcstring;
	break;
    }

    
    if( msg = LookupWidget(IntArg0(cmd),&widget,&class_id, NULL) )
	msg = msg;
    else if (string) {
	XtSetArg(args[n], XmNlabelType, XmSTRING); n++;
	XtSetArg(args[n], XmNlabelString, string); n++;
	XtSetValues(widget, args, n);
    }

    global.confirmed = 1;
    return (NULL);
}


/* ClmFcascadeButtonHighlight() */

ClmCommand *ClmFcascadeButtonHighlight(cmd)
ClmCommand *cmd;
{
	Widget widget;
	char *msg;

	if ((cmd->command != ClmCascadeButtonHighlight) || (cmd->num_arg != 2))
		GenError("ClmCascadeButtonHighlight: illegal command record");

	if (msg = LookupWidget(IntArg0(cmd), &widget, NULL, NULL))
		GenWarn(msg);

	if (XtIsSubclass (widget, xmCascadeButtonWidgetClass) ||
	    XtIsSubclass (widget, xmCascadeButtonGadgetClass))
	    XmCascadeButtonHighlight (widget, (IntArg1(cmd) != 0));
	else
	    GenWarn ("cascade-button-highlight: wrong widget class");

	if( global.forced_output )
	    XmUpdateDisplay(widget);
	return(NULL);
}


/* Contributed by: aihaug@AUSTIN.LOCKHEED.COM (Dan Haug) */
/* ==================================================
   Function EnableWithoutMouse
   ================================================== */

static Cursor hourglass = NULL;

ClmCommand *ClmFenableWithoutMouse(cmd)
ClmCommand *cmd;
{
  /* WidgetIntArg0() => toplevel widget id
     IntArg1 => if non-zero, grab pointer
     nothing is returned
     */
  Widget    wid;
  char   *  msg;
  int       grabp;

  if (msg = LookupWidget(IntArg0(cmd), &wid, NULL, NULL))
    GenError(msg);
  grabp = (int) IntArg1(cmd);


  /* fprintf(stderr,"EnableWithoutMouse wid=%x grabp=%d\n",wid,grabp); */
  if (!hourglass) {
    hourglass = XCreateFontCursor (XtDisplay(wid), 150);
  }
  if (grabp)
    XtGrabPointer(wid, False, NULL, grabp == 1 ? GrabModeSync : GrabModeAsync,
		 grabp == 1 ? GrabModeSync : GrabModeAsync, None,
                 hourglass, CurrentTime);
  return(NULL);
}

/* ==================================================
   Function DisableWithoutMouse
   ================================================== */


ClmCommand *ClmFdisableWithoutMouse(cmd)
ClmCommand *cmd;
{
  /* WidgetIntArg0() => toplevel widget id
     IntArg1 => if non-zero, ungrab pointer
     nothing is returned
     */
  Widget    wid;
  char   *  msg;
  int       ungrabp;

  if (msg = LookupWidget(IntArg0(cmd), &wid, NULL, NULL))
    GenError(msg);
  ungrabp = (int) IntArg1(cmd);

  if (ungrabp)
    XtUngrabPointer(wid, CurrentTime);
  return(NULL);
}

