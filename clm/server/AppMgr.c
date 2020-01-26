static char sccsid[] = "@(#)AppMgr.c	1.8 1/30/92";

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

ClmCommand *ClmFdestroyApplication(cmd) 
ClmCommand *cmd;
{
    if( cmd->command != ClmDestroyApplication || cmd->num_arg != 1 )
	GenError("ClmDestroyApplication: invalid command record");

    if( global.app_shell_id != IntArg0(cmd) )
	GenError("ClmDestroyApplication: application not found");

    XtDestroyWidget(global.app_shell_ptr);
    global.must_confirm_destroy++;
    XtCloseDisplay(global.display);
    global.app_shell_id = -1;
    global.app_shell_ptr = NULL;
    global.no_active_app = TRUE;
    return(NULL);
}

ClmCommand *ClmFcreateApplication(cmd) 
ClmCommand *cmd;
{
    /* Arguments:  0. shell_class 1. widget ID 2-n. args */

    char        *msg;
    int          class_id, ret_args;
    Arg         *arglist;
    WidgetID     ret_id;
    Widget       widget;

    if( cmd->command != ClmCreateApplication || cmd->num_arg < 2 ||
	(cmd->num_arg-2) % 2 )
	GenError("ClmFcreateShell: Invalid command record");

    if( global.app_shell_id != -1 )
	GenError("Application shell already created.");

    if( (class_id = ClassNameToIndex(SymbolArg0(cmd), &msg) ) == -1 )
	GenError(msg);
    
    if( (arglist = ClmMakeArgList(class_id, -1, -1, cmd->args+2, cmd->num_arg-2,
				  &msg, &ret_args, NULL)) == NULL)
	GenError(msg);

    widget = XtAppCreateShell(NULL, global.app_class,
			      ClassTable[class_id].class, 
			      global.display,
			      arglist, ret_args );
    ClmErrorTest();
    
    ret_id = EncapsulateWidget (widget, IntArg1(cmd), class_id, -1, &msg);
    if (ret_id < 0) {
	XtDestroyWidget (widget);
	GenError(msg);
    }

    global.app_shell_ptr = widget;
    global.app_shell_id = ret_id;
    global.no_active_app = FALSE;
    return(NULL);
}

static ClmArg MainLoopArgs[1];
static ClmCommand MainLoopCommand = {ClmEvent, -1, 1, MainLoopArgs};

void SendMainLoopTerminationMessage()
{
    MainLoopCommand.args[0].arg_type       = ClmArgInteger;
    MainLoopCommand.args[0].v.int_value    = ClmEventMainLoop;

    if(SendCommand(global.socket, &MainLoopCommand) == -1)
       abort();
}

ClmCommand *ClmFmainLoop(cmd)
ClmCommand *cmd;
{
    XEvent       event;
    char        *msg;

    if(cmd->command != ClmMainLoop || cmd->num_arg != 1)
        GenError("ClmMainLoop: Illegal command record");

    global.main_loop_level ++ ;

    for(;;) {
	if( global.closed && ! XtAppPending(global.app_context)) {
	    XmUpdateDisplay(global.app_shell_ptr);
	    global.closed = 0;
	    global.confirmed = TRUE;
	    global.main_loop_level -- ;
	    SendMainLoopTerminationMessage();
	    return(NULL);
	}
	XtAppNextEvent(global.app_context, &event);
	XtDispatchEvent(&event);
	if( global.must_confirm_destroy ) {
	    global.must_confirm_destroy--;
	    NotifyDestroyed();
	}
    }
    /* NOTREACHED */
}

ClmCommand *ClmFterminate(cmd)
ClmCommand *cmd;
{
    if( cmd->command != ClmTerminate)
	GenError("ClmTerminate: illegal command record");

    if( global.no_active_app )
       exit(0);
    global.terminated = TRUE;
    global.confirmed = TRUE;
    return(NULL);
}

ClmCommand *ClmFshutdown(cmd)
ClmCommand *cmd;
{
    global.closed = TRUE;
    return(NULL);
}

ClmCommand *ClmFclose(cmd)
ClmCommand *cmd;
{
    global.closed = TRUE;
    return(NULL);
}

ClmCommand *ClmFforcedOutputMode(cmd)
ClmCommand *cmd;
{
    if( cmd->command != ClmForcedOutputMode && cmd->num_arg != 1 )
	GenError("Illegal command record");

    if(IntArg0(cmd) > 0)
	global.forced_output++;
    else
	if( global.forced_output == 0 )
	    GenWarn("Forced output is already disabled")
	else
	    global.forced_output--;
    
    return(NULL);
}
