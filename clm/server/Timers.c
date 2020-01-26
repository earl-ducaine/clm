static char sccsid[] = "@(#)Timers.c	1.5 1/28/92";

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

#define INITIAL_TIMERS 1000

typedef struct _ClmTimer {
    unsigned long interval;
    XtIntervalId  id;
    Boolean       destroyed;
    Boolean       once;
    Boolean       enabled;
    Boolean       active_in_handler;
    Boolean       active_in_recursive_main_loops;
    Boolean       busy;
} ClmTimer;

static ClmTimer **TimerTable;

static int     first_free = -1;
static int     num_timers = 0;
static int     timer_alloc = INITIAL_TIMERS;

static ClmArg     HandlerArgs[2];
static ClmCommand HandlerCmd = {ClmEvent, -1, 2, HandlerArgs};

int RegisterTimer(clm_timer)
    ClmTimer *clm_timer;
{
    int new_id;

    if (num_timers == 0)
        TimerTable = (ClmTimer**)ClmMalloc(timer_alloc * sizeof(ClmTimer*));
    if (first_free == -1) {
        if (num_timers >= timer_alloc) {
            TimerTable = 
	        (ClmTimer**)XtRealloc(TimerTable, 
				      2*timer_alloc*sizeof(ClmTimer*));
            if (TimerTable == NULL) {
	       fprintf(stderr, "Fatal server error: Out of memory.\n");
	       fprintf(stderr, "Failed to allocate %d bytes for TimerTable.\n",
		       2*timer_alloc*sizeof(ClmTimer*));
	       fflush(stderr);
	       return (-1);}
            timer_alloc += timer_alloc;
	}
        new_id = num_timers++; }
    else {
        new_id = first_free;
        first_free = (int)TimerTable[first_free];
    }
    TimerTable[new_id] = clm_timer;
    return (new_id);
}

ClmTimer* FindTimer (timer_id)
    int timer_id;
{
    if (timer_id >= 0 && timer_id < num_timers)
       return TimerTable[timer_id];
    else
       return NULL;
}       

void UnregisterTimer (timer_id)
    int timer_id;
{
    if (timer_id >= 0 && timer_id < num_timers) {
       TimerTable[timer_id] = (ClmTimer*)first_free;
       first_free = timer_id;
    }
}

void TimerHandler(client_data, id)
caddr_t client_data;
XtIntervalId *id;
{
    ClmCommand *rc;
    ClmTimer *clm_timer = FindTimer((int)client_data);

    if (clm_timer == NULL) {
       fprintf(stderr, "Non-existant timer: %d\n", (int)client_data);
       fflush(stderr);
       abort ();
    }

    /* Send the timer only if it is enabled and not destroyed */
    
    if( clm_timer->enabled && ! clm_timer->destroyed ) {

	/*  Add timeout again if it is not once && should be active 
	 *  during the timer handler is running
	 */

	if( ! clm_timer->once && clm_timer->active_in_handler )
	    clm_timer->id = XtAppAddTimeOut(global.app_context,
					    clm_timer->interval,
					    (XtTimerCallbackProc)TimerHandler,
					    client_data);
	
	/* Mark timer busy during execution of the timer handler */
	if( global.main_loop_level == 0 ||
	    clm_timer->active_in_recursive_main_loops ) {

	    clm_timer->busy = True;
	    
	    HandlerCmd.args[0].arg_type       = ClmArgInteger;
	    HandlerCmd.args[0].v.int_value    =
		clm_timer->once ? ClmEventSimpleTimer : ClmEventTimer;
	    HandlerCmd.args[1].arg_type       = ClmArgInteger;
	    HandlerCmd.args[1].v.int_value    = (int) client_data;

	    if(SendCommand(global.socket, &HandlerCmd) == -1)
		abort();

	    /* free returned command */

	    ClmFreeCommand(ClmCallbackCommandLoop());
	    
	    clm_timer->busy = False;
        }

	/*  Add timeout again if it is not once && should be inactive 
	 *  during the timer handler is running
	 *  Note: The timer may have been destroyed during execution of
	 *        the timer handler !
	 *  Note: The timer may have been disabled during execution of
	 *        the timer handler !
	 */

	if(! clm_timer->once && ! clm_timer->active_in_handler &&
	   clm_timer->enabled && ! clm_timer->destroyed )
	    clm_timer->id = XtAppAddTimeOut(global.app_context,
					    clm_timer->interval,
					    (XtTimerCallbackProc)TimerHandler,
					    client_data);

	/* Necessary if start+destroy are called in the Timer callback */
	if( clm_timer->enabled && clm_timer->destroyed )
	    XtRemoveTimeOut((XtIntervalId)clm_timer->id);

    }
    else {
	if( clm_timer->destroyed ) {
	    /* I don't expect this to happen (Just for security) */
	    /*fprintf(stderr,"Warning: The timer has already been destroyed!\n");
	    fflush(stderr);*/
	}
    }

    if( clm_timer->once || clm_timer->destroyed ) {
        UnregisterTimer((int)client_data);
        XtFree((char *)clm_timer);
    }
}

ClmCommand *ClmFcreateTimer(cmd)
ClmCommand *cmd;
{
    /* IntArg0()  => Timeout value in milliseconds.
       IntArg1()  => Boolean flag which indicates whether the timer should be
		     set again after it has expired.
       IntArg2()  => Boolean flag which indicates whether the timer should be
		     active while the timer handler is executing.
       IntArg3()  => Boolean flag which indicates whether the timer should be
		     active in recursive main loops.
       IntArg4()  => Boolean flag which indicates whether the timer is
		     initially enabled or disabled.
    */

    ClmTimer         *clm_timer;
    int               new_id;
    static ClmCommand   command;
    static ClmArg       args[1];
    
    if( cmd->command != ClmCreateTimer || cmd->num_arg != 5 ) 
        GenError("ClmCreateTimer: illegal command record");

    if( IntArg0(cmd) <= 0 )
	GenWarn("Timeout interval must be greater 0");
    
    clm_timer = (ClmTimer *)ClmMalloc(sizeof(ClmTimer));

    new_id = RegisterTimer(clm_timer);
    if (new_id < 0)
       GenWarn ("Failed to register timer");

    clm_timer->interval = IntArg0(cmd);
    clm_timer->destroyed = False;
    clm_timer->once = IntArg1(cmd) ? True : False;
    clm_timer->active_in_handler = IntArg2(cmd) ? True : False;
    clm_timer->active_in_recursive_main_loops = IntArg3(cmd) ? True : False;
    clm_timer->enabled = IntArgn(cmd,4) ? True : False;

    if( clm_timer->enabled ) {
	clm_timer->id = XtAppAddTimeOut(global.app_context,
					clm_timer->interval,
					(XtTimerCallbackProc)TimerHandler,
					(caddr_t)new_id);
    }

    command.command = ClmReturnValues;
    command.num_arg = 1;
    command.args    = args;
    args[0].arg_type = ClmArgInteger;
    args[0].v.int_value = new_id;
    return(&command);
}

ClmCommand *ClmFdestroyTimer(cmd)
ClmCommand *cmd;
{
    /* IntArg0()  => ClmTimer to remove */

    ClmTimer *clm_timer;

    if( cmd->command != ClmDestroyTimer || cmd->num_arg != 1 ) 
        GenError("ClmDestroyTimer: illegal command record");

    if ((clm_timer = FindTimer(IntArg0(cmd))) == NULL)
        GenWarn ("Illegal timer ID for destroy");
    /* clm_timer = (ClmTimer *)IntArg0(cmd); */
    
    clm_timer->destroyed = True;
    if(clm_timer->enabled &&
       ( clm_timer->active_in_handler || ! clm_timer->busy ) )
	XtRemoveTimeOut((XtIntervalId)clm_timer->id);

    /*if( ! clm_timer->busy )
	XtFree(clm_timer);*/
    
    return(NULL);
}

ClmCommand *ClmFchangeTimer(cmd)
ClmCommand *cmd;
{
    /*  IntArg0()  => ClmTimer to change
	IntArg1()  => new interval
     */

    ClmTimer *clm_timer;

    if( cmd->command != ClmChangeTimer || cmd->num_arg != 2 ) 
        GenError("ClmChangeTimeout: illegal command record");

    if ((clm_timer = FindTimer(IntArg0(cmd))) == NULL)
        GenWarn ("Illegal timer ID for change");
    /* clm_timer = (ClmTimer *)IntArg0(cmd); */
    
    if( IntArg1(cmd) <= 0 )
	GenWarn("Timeout interval must be greater 0");
    
    if( clm_timer->destroyed )
	GenWarn("Timeout is already destroyed");
    
    clm_timer->interval = IntArg1(cmd);

    return(NULL);
}

ClmCommand *ClmFrestartTimer(cmd)
ClmCommand *cmd;
{
    /*  IntArg0()  => ClmTimer to restart
	IntArg1()  => new interval (if != -1 )
     */

    ClmTimer *clm_timer;

    if( cmd->command != ClmRestartTimer || cmd->num_arg != 2 ) 
        GenError("ClmRestartTimeout: illegal command record");

    if ((clm_timer = FindTimer(IntArg0(cmd))) == NULL)
        GenWarn ("Illegal timer ID for restart");
    /* clm_timer = (ClmTimer *)IntArg0(cmd); */
    
    if( clm_timer->destroyed )
	GenWarn("Timeout has already been destroyed!");

    if( clm_timer->enabled )
	GenWarn("Timeout has already been actived!");

    /* Test for a valid interval */
    if( IntArg1(cmd) != -1 && IntArg1(cmd) <= 0 )
	GenWarn("Timeout interval must be greater 0");

    if( IntArg1(cmd) > 0 )
	clm_timer->interval = IntArg1(cmd);
    
    clm_timer->id = XtAppAddTimeOut(global.app_context,
				    clm_timer->interval,
				    (XtTimerCallbackProc)TimerHandler,
				    IntArg0(cmd));
    clm_timer->enabled = True;
    return(NULL);
}

ClmCommand *ClmFstopTimer(cmd)
ClmCommand *cmd;
{
    /*  IntArg0()  => ClmTimer to stop */
    
    ClmTimer *clm_timer;

    if( cmd->command != ClmStopTimer || cmd->num_arg != 1 ) 
        GenError("ClmStopTimeout: illegal command record");
    
    if ((clm_timer = FindTimer(IntArg0(cmd))) == NULL)
        GenWarn ("Illegal timer ID for stop");
    /* clm_timer = (ClmTimer *)IntArg0(cmd); */
    
    if( clm_timer->destroyed )
	GenWarn("Timeout has already been destroyed!");

    if( ! clm_timer->enabled )
	GenWarn("Timeout has already been deactived!");

    XtRemoveTimeOut((XtIntervalId)clm_timer->id);
    clm_timer->enabled = False;
    return(NULL);
}





