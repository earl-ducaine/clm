static char sccsid[] = "@(#)GeometryMgr.c	1.6 1/28/92";

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

#include <X11/IntrinsicP.h>
#include <X11/StringDefs.h>

#include "interface.h"
#include "functions.h"

ClmCommand *ClmFmove(cmd)
ClmCommand *cmd;
{
    Widget widget;
    WidgetClass class;
    char  *msg;

    if( cmd->command != ClmMove || cmd->num_arg != 3 )
	GenError("ClmMove: illegal command record");

    if( msg = LookupWidget(IntArg0(cmd), &widget, &class, NULL) )
	GenWarn(msg);

    XtMoveWidget(widget, IntArg1(cmd), IntArg2(cmd));
    if( global.forced_output )
	XmUpdateDisplay(widget);
    ClmErrorTest();
    return(NULL);
}

ClmCommand *ClmFresize(cmd)
ClmCommand *cmd;
{
    Widget widget;
    WidgetClass class;
    char  *msg;

    if( cmd->command != ClmResize || cmd->num_arg != 4 )
	GenError("ClmResize: illegal command record");

    if( msg = LookupWidget(IntArg0(cmd), &widget, &class, NULL) )
	GenWarn(msg);

    XtResizeWidget(widget, IntArg1(cmd), IntArg2(cmd), IntArg3(cmd) );
    if( global.forced_output )
	XmUpdateDisplay(widget);
    ClmErrorTest();
    return(NULL);
}

ClmCommand *ClmFconfigure(cmd)
ClmCommand *cmd;
{
    Widget widget;
    WidgetClass class;
    char  *msg;

    if( cmd->command != ClmConfigure || cmd->num_arg != 6 )
	GenError("ClmConfigure: illegal command record");

    if( msg = LookupWidget(IntArg0(cmd), &widget, &class, NULL) )
	GenWarn(msg);

    XtConfigureWidget(widget, IntArg1(cmd), IntArg2(cmd), IntArg3(cmd), 
			      IntArgn(cmd,4), IntArgn(cmd,5) );
    if( global.forced_output )
	XmUpdateDisplay(widget);
    ClmErrorTest();
    return(NULL);
}

static ClmArg tc_args[2];
static ClmCommand tc_cmd = { ClmReturnValues, -1, 2, tc_args };

ClmCommand *ClmFtranslateCoords(cmd)
ClmCommand *cmd;
{
    char     *msg;
    Widget    widget;
    Position  ret_x, ret_y;

    if( cmd->command != ClmTranslateCoords || cmd->num_arg != 3 )
	GenError("ClmTranslateCoords: illegal command record");

    if( msg = LookupWidget(IntArg0(cmd), &widget, NULL, NULL) )
	GenWarn(msg);

    XtTranslateCoords(widget, (Position)(IntArg1(cmd)),
			      (Position)(IntArg2(cmd)),
			      &ret_x, &ret_y);

    tc_cmd.args[0].arg_type    = ClmArgInteger;
    tc_cmd.args[0].v.int_value = (int)ret_x;
    tc_cmd.args[1].arg_type    = ClmArgInteger;
    tc_cmd.args[1].v.int_value = (int)ret_y;

    return(&tc_cmd);
}

static ClmArg gss_args[4];
static ClmCommand gss_cmd = {ClmReturnValues, -1, 4, gss_args};

ClmCommand *ClmFgetScreenSize(cmd)
ClmCommand *cmd;
{
    char  *msg;
    int    x_pixel, y_pixel, x_mm, y_mm;

    if( cmd->command != ClmGetScreenSize || cmd->num_arg != 2 )
	GenError("get-screen-size: illegal command record");

    gss_cmd.args[0].arg_type = ClmArgInteger;
    gss_cmd.args[0].v.int_value = DisplayWidth(global.display, IntArg1(cmd));
    gss_cmd.args[1].arg_type = ClmArgInteger;
    gss_cmd.args[1].v.int_value = DisplayHeight(global.display, IntArg1(cmd));
    gss_cmd.args[2].arg_type = ClmArgInteger;
    gss_cmd.args[2].v.int_value = DisplayWidthMM(global.display, IntArg1(cmd));
    gss_cmd.args[3].arg_type = ClmArgInteger;
    gss_cmd.args[3].v.int_value = DisplayHeightMM(global.display, IntArg1(cmd));

    return(&gss_cmd);
}
