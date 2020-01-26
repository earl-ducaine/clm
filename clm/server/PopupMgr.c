static char sccsid[] = "@(#)PopupMgr.c	1.6 1/28/92";

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

static XtGrabKind PopupGrabs[3] = 
    { XtGrabNone, XtGrabNonexclusive, XtGrabExclusive };

ClmCommand *ClmFpopup(cmd)
ClmCommand *cmd;
{
    WidgetClass class;
    char       *msg;
    Widget widget;
    XtGrabKind GrabKind;

    if( cmd->command != ClmPopup || cmd->num_arg != 2 )
	GenError("ClmPopup: illegal command record");

    if( msg = LookupWidget(IntArg0(cmd),&widget,&class, NULL) )
        GenWarn(msg);

    if( ! strcmp( SymbolArg1(cmd), "GRAB-EXCLUSIVE"))
	GrabKind = XtGrabExclusive;
    else if( ! strcmp( SymbolArg1(cmd), "GRAB-NONE"))
	     GrabKind = XtGrabNone;
	 else if( ! strcmp( SymbolArg1(cmd), "GRAB-NONEXCLUSIVE"))
		  GrabKind = XtGrabNonexclusive;
	      else GenWarn("Invalid GRAB-KIND");

    XtPopup(widget, GrabKind);
    if( global.forced_output )
	XmUpdateDisplay(widget);
    return(NULL);
}

ClmCommand *ClmFmanagePopupChild(cmd)
ClmCommand *cmd;
{
    WidgetClass class;
    char       *msg;
    Widget widget;

    if( cmd->command != ClmManagePopupChild || cmd->num_arg != 1 )
	GenError("ClmManagePopupChild: illegal command record");

    if( msg = LookupWidget(IntArg0(cmd),&widget,&class, NULL) )
        GenWarn(msg);

    XmUpdateDisplay(widget);
    XtManageChild(widget);
    XmUpdateDisplay(widget);
    return(NULL);
}

ClmCommand *ClmFpopdown(cmd)
ClmCommand *cmd;
{
    WidgetClass class;
    char       *msg;
    Widget widget;

    if( cmd->command != ClmPopdown || cmd->num_arg != 1 )
	GenError("ClmPopdown: illegal command record");

    if( msg = LookupWidget(IntArg0(cmd),&widget,&class, NULL) )
        GenWarn(msg);

    XtPopdown(widget);
    if( global.forced_output )
	XmUpdateDisplay(widget);
    return(NULL);
}

ClmCommand *ClmFunmanagePopupChild(cmd)
ClmCommand *cmd;
{
           WidgetClass class;
           char       *msg;
           Widget      widget;

    if( cmd->command != ClmUnmanagePopupChild || cmd->num_arg != 1 )
	GenError("ClmUnmanagePopupChild: illegal command record");

    if( msg = LookupWidget(IntArg0(cmd),&widget,&class, NULL) )
        GenWarn(msg);

    XtUnmanageChild(widget);
    XmUpdateDisplay(widget);
    return(NULL);
}
