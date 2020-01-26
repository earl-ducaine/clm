static char sccsid[] = "@(#)ActionMgr.c	1.8 1/30/92";

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
 * Authors: Thomas Spandoeck
 *          VW-GEDAS
 *          Pascalstr. 11
 *          D-1000 Berlin 10
 *
 *          Andreas Baecker (baecker@gmdzi.gmd.de)
 *          P.O. Box 1316
 *          D-5205 Sankt Augustin 1
 */

#include <stdio.h>
#include <sys/types.h>

#include <X11/IntrinsicP.h>
#include <X11/StringDefs.h>
#include <Xm/Xm.h>

#include "interface.h"
#include "functions.h"

#define MAX_ACTIONS_ARG 50

static ClmArg     HandlerArgs[MAX_ACTIONS_ARG];
static ClmCommand HandlerCmd = {ClmEvent, 0, 0, HandlerArgs};

void ClmAction(widget,event,params,num_params)
Widget widget;
XEvent *event;
String *params;
Cardinal *num_params;
{
    int         i, class, parent_class;
    WidgetID    widget_id;
    ClmCommand *rc;
    char       *msg;

    /*
    fprintf(stderr, "Action: widget (%d) event (%x) num_params = (%d)\n",
	    widget, event, *num_params);
    for(i=0; i < *num_params; i++)
	fprintf(stderr, "Param[%d] = %s\n", i, params[i]);
    fflush(stderr);
    */

    widget_id = EncapsulateWidget(widget, EW_GENERATE_ID_IF_NECESSARY,
				  EW_COMPUTE_CLASS_ID, EW_COMPUTE_CLASS_ID,
				  &msg);
    if (widget_id < 0) {
	fprintf(stderr, "ClmAction: %s\n", msg);
	abort();
    }

    HandlerCmd.args[0].arg_type       = ClmArgInteger;
    HandlerCmd.args[0].v.int_value    = ClmEventAction;
    HandlerCmd.args[1].arg_type       = ClmArgInteger;
    HandlerCmd.args[1].v.int_value    = widget_id;
    HandlerCmd.args[2].arg_type       = ClmArgInteger;
    HandlerCmd.args[2].v.int_value    = *num_params;
    for(i=0; i < *num_params; i++) {
	HandlerCmd.args[3 + i].arg_type   = ClmArgString;
	HandlerCmd.args[3 + i].v.string_value = params[i];
    }
    HandlerCmd.args[3 + *num_params].arg_type = ClmArgInteger;
    HandlerCmd.args[3 + *num_params].v.int_value = event->type;
    HandlerCmd.num_arg = 3 + *num_params + 1 +
      FillEventData(HandlerCmd.args + 3 + *num_params + 1, event);

    if(HandlerCmd.num_arg > MAX_ACTIONS_ARG) {
	fprintf(stderr, "Too many action parameters in \"clm\" action\n");
	fflush(stderr);
	abort();
    }
    if(SendCommand(global.socket, &HandlerCmd) == -1)
       abort();
		 
    ClmFreeCommand(ClmCallbackCommandLoop());
}
