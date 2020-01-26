static char sccsid[] = "@(#)Events.c	1.7 9/8/93";

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
#include <Xm/Xm.h>
#include <Xm/ScrollBar.h>
#include <Xm/List.h>
#include <Xm/Command.h>
#include <Xm/DrawingA.h>
#include <Xm/DrawnB.h>
#include <Xm/ToggleB.h>
#include <Xm/Scale.h>
#include <Xm/SelectioB.h>
#include <Xm/FileSB.h>
#include <Xm/Text.h>
#include <Xm/Xm.h>

#include <Xm/PushB.h>

#include "interface.h"
#include "functions.h"

typedef struct ClmEI ClmEventInfo;

struct ClmEI {
    EventMask         event_mask;           /* Mask used in Lisp-Hash-Table */
    int               nonmaskable;         
    WidgetID          widget_id;            /* name of widget */
    Widget            widget;               /* pointer to widget */
    WidgetNode       *node;
    ClmEventInfo     *next, *pre;           /* chain pointers */
};

static ClmEventInfo EventList = { 0, 0, 0, NULL, NULL, &EventList, &EventList };

static ClmCommand HandlerCmd = {ClmEvent, -1, 0, NULL};

void SaveEvent(event, queue)
XEvent     *event;
EventNode **queue;
{
    EventNode *saved_node;

    saved_node = (EventNode *)ClmMalloc(sizeof(EventNode));
    saved_node->next = *queue;
    *queue = saved_node;
    bcopy(event, &saved_node->event, sizeof(XEvent));
}

void FreeEventNodes(queue)
EventNode **queue;
{
    register EventNode *node, *next;

    for(node = next = *queue; node; node = next) {
	next = node->next;
        XtFree(node);
    }
    *queue = NULL;
}

int FillExposeData(arg_ptr, n_expose, node)
ClmArg    *arg_ptr;
int        n_expose;
register EventNode *node;
{
    register ClmArg *ea, *a = arg_ptr;

    a->arg_type = ClmArgInteger;
    a->v.int_value = n_expose; a++;

    /* Put in Events in reverse order (Not really necessary) */
    for(ea = a + (4 * (n_expose-1)); node; node = node->next, ea -= 8) {
        ea->arg_type = ClmArgInteger;
        ea->v.int_value = node->event.xexpose.x; ea++;
        ea->arg_type = ClmArgInteger;
        ea->v.int_value = node->event.xexpose.y; ea++;
        ea->arg_type = ClmArgInteger;
        ea->v.int_value = node->event.xexpose.width; ea++;
        ea->arg_type = ClmArgInteger;
        ea->v.int_value = node->event.xexpose.height; ea++;
    }
    return(4*n_expose+1);
}

int FillMotionData(arg_ptr, n_motion, node)
ClmArg    *arg_ptr;
int        n_motion;
register EventNode *node;
{
    register ClmArg *ea, *a = arg_ptr;

    a->arg_type = ClmArgInteger;
    a->v.int_value = n_motion; a++;

    /* Put in Events in reverse order (Not really necessary) */
    for(ea = a + (5 * (n_motion-1)); node; node = node->next, ea -= 10) {
	ea->arg_type = ClmArgInteger;
	ea->v.int_value = node->event.xmotion.x; ea++;
	ea->arg_type = ClmArgInteger;
	ea->v.int_value = node->event.xmotion.y; ea++;
	ea->arg_type = ClmArgInteger;
	ea->v.int_value = node->event.xmotion.x_root; ea++;
	ea->arg_type = ClmArgInteger;
	ea->v.int_value = node->event.xmotion.y_root; ea++;
	ea->arg_type = ClmArgInteger;
	ea->v.int_value = node->event.xmotion.state; ea++;
    }
    return(5*n_motion+1);
}

int FillEventData(arg_ptr,event)
ClmArg *arg_ptr;
XEvent *event;
{
    ClmArg *a = arg_ptr;
    static char key_string[50];
    KeySym key_sym;
    int i;

    switch(event->type) {
	case Expose:
	    /* report count, x, y, width, height */
	          a->arg_type = ClmArgInteger;
	          a->v.int_value = 1;            /* One region to be exposed */
	     a++; a->arg_type = ClmArgInteger;
	          a->v.int_value = event->xexpose.x;
	     a++; a->arg_type = ClmArgInteger;
	          a->v.int_value = event->xexpose.y;
	     a++; a->arg_type = ClmArgInteger;
	          a->v.int_value = event->xexpose.width;
	     a++; a->arg_type = ClmArgInteger;
	          a->v.int_value = event->xexpose.height;
	     break;
        case KeyPress:
        case KeyRelease:
            /* report x y x_root y_root state button */
                 a->arg_type = ClmArgInteger;
                 a->v.int_value = event->xkey.x;
            a++; a->arg_type = ClmArgInteger;
                 a->v.int_value = event->xkey.y;
            a++; a->arg_type = ClmArgInteger;
                 a->v.int_value = event->xkey.x_root;
            a++; a->arg_type = ClmArgInteger;
                 a->v.int_value = event->xkey.y_root;
            a++; a->arg_type = ClmArgInteger;
                 a->v.int_value = event->xkey.state;
            a++; a->arg_type = ClmArgInteger;
                 a->v.int_value = event->xkey.keycode;
	    a++; a->arg_type = ClmArgString;
            XLookupString((XKeyEvent *)event, key_string, 50, &key_sym, NULL);
	    a->v.string_value = XKeysymToString(key_sym);
	    if( !  a->v.string_value )
		 a->v.string_value = "";
            break;
        case ButtonPress:
	case ButtonRelease:
            /* report x y x_root y_root state button */
                 a->arg_type = ClmArgInteger;
                 a->v.int_value = event->xbutton.x;
            a++; a->arg_type = ClmArgInteger;
                 a->v.int_value = event->xbutton.y;
            a++; a->arg_type = ClmArgInteger;
                 a->v.int_value = event->xbutton.x_root;
            a++; a->arg_type = ClmArgInteger;
                 a->v.int_value = event->xbutton.y_root;
            a++; a->arg_type = ClmArgInteger;
                 a->v.int_value = event->xbutton.state;
            a++; a->arg_type = ClmArgInteger;
                 a->v.int_value = event->xbutton.button;
            a++; a->arg_type = ClmArgInteger;
                 a->v.int_value = (int) (event->xbutton.time & 0x7fffffff);
            break;
        case MotionNotify:
            /* report x y x_root y_root state */
                 a->arg_type = ClmArgInteger;
                 a->v.int_value = 1;         /* One Motion event */
            a++; a->arg_type = ClmArgInteger;
                 a->v.int_value = event->xmotion.x;
            a++; a->arg_type = ClmArgInteger;
                 a->v.int_value = event->xmotion.y;
            a++; a->arg_type = ClmArgInteger;
                 a->v.int_value = event->xmotion.x_root;
            a++; a->arg_type = ClmArgInteger;
                 a->v.int_value = event->xmotion.y_root;
            a++; a->arg_type = ClmArgInteger;
                 a->v.int_value = event->xmotion.state;
            break;
        case EnterNotify:
        case LeaveNotify:
            /* report x y x_root y_root mode detail focus state */
                 a->arg_type = ClmArgInteger;
                 a->v.int_value = event->xcrossing.x;
            a++; a->arg_type = ClmArgInteger;
                 a->v.int_value = event->xcrossing.y;
            a++; a->arg_type = ClmArgInteger;
                 a->v.int_value = event->xcrossing.x_root;
            a++; a->arg_type = ClmArgInteger;
                 a->v.int_value = event->xcrossing.y_root;
            a++; a->arg_type = ClmArgInteger;
                 a->v.int_value = event->xcrossing.mode;
            a++; a->arg_type = ClmArgInteger;
                 a->v.int_value = event->xcrossing.detail;
            a++; a->arg_type = ClmArgInteger;
                 a->v.int_value = event->xcrossing.focus;
            a++; a->arg_type = ClmArgInteger;
                 a->v.int_value = event->xcrossing.state;
            break;
	case FocusIn:
	case FocusOut:
	    /* report mode detail */
                 a->arg_type = ClmArgInteger;
                 a->v.int_value = event->xfocus.mode;
            a++; a->arg_type = ClmArgInteger;
                 a->v.int_value = event->xfocus.detail;
            break;
	case ConfigureNotify:
            /* report x y width height */
                 a->arg_type = ClmArgInteger;
                 a->v.int_value = event->xconfigure.x;
            a++; a->arg_type = ClmArgInteger;
                 a->v.int_value = event->xconfigure.y;
            a++; a->arg_type = ClmArgInteger;
                 a->v.int_value = event->xconfigure.width;
            a++; a->arg_type = ClmArgInteger;
                 a->v.int_value = event->xconfigure.height;
            break;
	case VisibilityNotify:
            /* report state */
                 a->arg_type = ClmArgInteger;
                 a->v.int_value = event->xvisibility.state;
            break;
        case PropertyNotify:
            /* report atom state time */
                 a->arg_type = ClmArgInteger;
                 a->v.int_value = event->xproperty.atom;
            a++; a->arg_type = ClmArgInteger;
                 a->v.int_value = event->xproperty.state;
            a++; a->arg_type = ClmArgInteger;
                 a->v.int_value = event->xproperty.time & 0x7fffffff;
            break;
	case SelectionClear:
            /* report selection time */
                 a->arg_type = ClmArgInteger;
                 a->v.int_value = event->xselectionclear.selection;
            a++; a->arg_type = ClmArgInteger;
                 a->v.int_value = event->xselectionclear.time & 0x7fffffff;
            break;
	case SelectionRequest:
            /* report requestor selection target property time */
                 a->arg_type = ClmArgInteger;
                 a->v.int_value = event->xselectionrequest.requestor;
            a++; a->arg_type = ClmArgInteger;
                 a->v.int_value = event->xselectionrequest.selection;
            a++; a->arg_type = ClmArgInteger;
                 a->v.int_value = event->xselectionrequest.target;
            a++; a->arg_type = ClmArgInteger;
                 a->v.int_value = event->xselectionrequest.property;
            a++; a->arg_type = ClmArgInteger;
                 a->v.int_value = event->xselectionrequest.time & 0x7fffffff;
            break;
	case SelectionNotify:
            /* report selection target property time */
                 a->arg_type = ClmArgInteger;
                 a->v.int_value = event->xselection.selection;
            a++; a->arg_type = ClmArgInteger;
                 a->v.int_value = event->xselection.target;
            a++; a->arg_type = ClmArgInteger;
                 a->v.int_value = event->xselection.property;
            a++; a->arg_type = ClmArgInteger;
                 a->v.int_value = event->xselection.time & 0x7fffffff;
            break;
	case ClientMessage:
            a->arg_type = ClmArgInteger;
            a->v.int_value = event->xclient.message_type;
            if (event->xclient.format == 32)
	       for (i=0; i<5; i++) {
                   a++; a->arg_type = ClmArgInteger;
                        a->v.int_value = event->xclient.data.l[i];
               };
            if (event->xclient.format == 16)
               for (i=0; i<10; i++) {
                   a++; a->arg_type = ClmArgInteger;
                        a->v.int_value = event->xclient.data.s[i];
               };
            if (event->xclient.format == 8)
               for (i=0; i<20; i++) {
                   a++; a->arg_type = ClmArgInteger;
                        a->v.int_value = event->xclient.data.b[i];
               };
            break;
        default:
            a--;
            break;
    }
    return(a-arg_ptr+1);
}

static Window MotionWindow;

Bool MotionEventInQueue(display, event, arg)
Display *display;
XEvent *event;
char *arg;
{
    return( event->type == MotionNotify && 
	    event->xmotion.window == MotionWindow ? True : False);
}

void DefaultEventHandler(widget, client_data, event)
Widget   widget;
ClmEventInfo  *client_data;
XEvent  *event;
{
           ClmCommand *rc;
           XEvent      event_return;
	   int         n_queued, min_args;
	   EventNode  *e_node;
    static int         n_args = 0;

    min_args = 25; /* At least 25 Arguments */

    if(event->type == Expose) {
	/*fprintf(stderr, "Saving Expose event\n"); fflush(stderr);*/
	SaveEvent(event, &client_data->node->exposures);
	if(event->xexpose.count > 0 &&
	   ! widget->core.widget_class->core_class.compress_exposure)
	    return;
	else {
	    /*fprintf(stderr, "Count == 0\n"); fflush(stderr);*/
	    for(e_node = client_data->node->exposures, n_queued = 0;
		e_node;
		e_node = e_node->next, min_args += 4, n_queued++);
	}
    }

    if(event->type == MotionNotify ) {
	MotionWindow = event->xmotion.window;
	/*fprintf(stderr, "Saving Motion event\n"); fflush(stderr);*/
	SaveEvent(event, &client_data->node->motions);
	/*
	if( XPeekIfEvent(global.display, &event_return,
			 MotionEventInQueue, NULL) ) {
	    return;
        }
	else {
	    fprintf(stderr, "Sending buffered motion events\n"); fflush(stderr);
	*/
	    for(e_node = client_data->node->motions, n_queued = 0;
		e_node;
		e_node = e_node->next, min_args += 5, n_queued++);
	/*  } */
    }

    if( min_args > n_args ) {
	XtFree(HandlerCmd.args);
	HandlerCmd.args = (ClmArg *)ClmMalloc(min_args*sizeof(ClmArg));
	n_args = min_args;
    }

    HandlerCmd.args[0].arg_type       = ClmArgInteger;
    HandlerCmd.args[0].v.int_value    = ClmEventEvent;
    HandlerCmd.args[1].arg_type       = ClmArgInteger;
    HandlerCmd.args[1].v.int_value    = client_data->widget_id;
    HandlerCmd.args[2].arg_type       = ClmArgInteger;
    HandlerCmd.args[2].v.int_value    = client_data->event_mask;
    HandlerCmd.args[3].arg_type       = ClmArgInteger;
    HandlerCmd.args[3].v.int_value    = event->type;

    HandlerCmd.num_arg = 4;
    switch(event->type) {
        case Expose:
            HandlerCmd.num_arg += FillExposeData(HandlerCmd.args+4, n_queued,
						 client_data->node->exposures);
	    FreeEventNodes(&client_data->node->exposures);
	    break;
	case MotionNotify:
	    HandlerCmd.num_arg += FillMotionData(HandlerCmd.args+4, n_queued,
						 client_data->node->motions);
	    FreeEventNodes(&client_data->node->motions);
	    break;
	default:
	     HandlerCmd.num_arg += FillEventData(HandlerCmd.args+4, event);
	     break;
    }

    if( min_args < HandlerCmd.num_arg ) {
	fprintf(stderr,"Fatal error: Argument list %d %d (consult guru)\n",
                       event->type, HandlerCmd.num_arg);
	abort();
    }

    if(SendCommand(global.socket, &HandlerCmd) == -1)
       abort();

    /* free returned command */

    ClmFreeCommand(ClmCallbackCommandLoop());
}

char *ClmEventAdd(widget, widget_id, event_mask, nonmaskable, node)
Widget      widget;
WidgetID    widget_id;
EventMask   event_mask;
int         nonmaskable;
WidgetNode *node;
{
    ClmEventInfo      *info;
    static char                  err_msg[100];


    info = (ClmEventInfo *)ClmMalloc(sizeof(ClmEventInfo));
    
    info->widget_id       = widget_id;
    info->event_mask      = event_mask;
    info->nonmaskable     = nonmaskable;
    info->widget          = widget;
    info->node            = node;
    info->pre             = &EventList;
    info->next            = EventList.next;
    EventList.next->pre   = info;
    EventList.next        = info;

    XtAddEventHandler(widget, info->event_mask, info->nonmaskable,
		      DefaultEventHandler, info);

    return(NULL);
}

char *ClmEventRemove( widget, widget_id, event_mask, nonmaskable, node)
Widget    widget;
WidgetID  widget_id;
EventMask event_mask;
int       nonmaskable;
caddr_t   node;
{
    ClmEventInfo *info;

    for( info = EventList.next; info != &EventList; info = info->next )
        if( info->widget == widget && widget_id == info->widget_id
            && event_mask == info->event_mask
	    && nonmaskable == info->nonmaskable)
            break;
    
    if( info == &EventList )
        return("Event-Handler-Info not found");

    XtRemoveEventHandler(widget, info->event_mask, info->nonmaskable,
			 DefaultEventHandler, info);
    
    info->pre->next = info->next;
    info->next->pre = info->pre;
    free(info);
    return(NULL);
}

