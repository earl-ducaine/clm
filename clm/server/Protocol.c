static char sccsid[] = "@(#)Protocol.c	1.7 9/8/93";

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
#if XmREVISION >= 2
#include <Xm/Protocols.h>
#else
#include <X11/Protocols.h>
#endif

#include "interface.h"
#include "functions.h"

typedef struct ClmPCBI ClmProtocolCallbackInfo;

struct ClmPCBI {
    Atom                     property,
			     protocol;
    char                    *property_name,
			    *protocol_name;
    WidgetID                 widget_id;
    Widget                   widget;
    ClmProtocolCallbackInfo *next, *pre;
};

static ClmProtocolCallbackInfo CallbackList = { NULL, NULL, NULL, NULL, 0, NULL,
					        &CallbackList, &CallbackList };

static Atom MakeProtocolAtom(widget, name)
Widget  widget;
char   *name;
{
    register char *s;
    register int   i, len;

    len = strlen(name);

    s = ClmMalloc(len+1);
    strcpy(s,name);

    for(i=0; i<len; ++i)
	if(s[i] == '-')
	    s[i] = '_';
    return(XmInternAtom(XtDisplay(widget), s, False));
}

static char *StringCopy(s)
char *s;
{
    register char *copy;

    copy = ClmMalloc(strlen(s)+1);
    strcpy(copy, s);
    return(copy);
}

static ClmArg HandlerArgs[4];
static ClmCommand HandlerCmd = {ClmEvent, -1, 4, HandlerArgs};

static void ClmProtocolCallbackHandler(widget, client_data, call_data)
Widget                   widget;
ClmProtocolCallbackInfo *client_data;
caddr_t          call_data;
{
    ClmCommand *rc;

    HandlerCmd.args[0].arg_type       = ClmArgInteger;
    HandlerCmd.args[0].v.int_value    = ClmEventProtocol;
    HandlerCmd.args[1].arg_type       = ClmArgInteger;
    HandlerCmd.args[1].v.int_value    = client_data->widget_id;
    HandlerCmd.args[2].arg_type       = ClmArgSymbol;
    HandlerCmd.args[2].v.symbol_value = client_data->property_name;
    HandlerCmd.args[3].arg_type       = ClmArgSymbol;
    HandlerCmd.args[3].v.symbol_value = client_data->protocol_name;

    if(SendCommand(global.socket, &HandlerCmd) == -1)
       abort();

    rc = ClmCallbackCommandLoop();
} 

char *ClmProtocolCallbackAdd(widget, widget_id, property_name, protocol_name)
Widget    widget;
WidgetID  widget_id;
char     *property_name;
char     *protocol_name;
{
    static char                     err_msg[100];
           ClmProtocolCallbackInfo *info;

    info = 
      (ClmProtocolCallbackInfo *)ClmMalloc(sizeof(ClmProtocolCallbackInfo));
    
    info->widget_id       = widget_id;
    info->widget          = widget;
    info->property        = MakeProtocolAtom(widget, property_name);
    info->protocol        = MakeProtocolAtom(widget, protocol_name);
    info->property_name   = StringCopy(property_name);
    info->protocol_name   = StringCopy(protocol_name);

    info->pre              = &CallbackList;
    info->next             = CallbackList.next;
    CallbackList.next->pre = info;
    CallbackList.next      = info;

    XmAddProtocolCallback(widget, info->property, info->protocol,
			          ClmProtocolCallbackHandler, info);
    return(NULL);
}

char *ClmProtocolCallbackRemove(widget, widget_id, property, protocol)
Widget    widget;
WidgetID  widget_id;
char     *property, *protocol;
{
    ClmProtocolCallbackInfo *info;

    for( info = CallbackList.next; info != &CallbackList; info = info->next )
	if( info->widget == widget && widget_id == info->widget_id
	    && ! strcmp(property, info->property_name)
	    && ! strcmp(protocol, info->protocol_name) )
	    break;
    
    if( info == &CallbackList )
	return("Property callback not found");

    XmRemoveProtocolCallback(widget, info->property, info->protocol,
			     ClmProtocolCallbackHandler, info);
    if( global.error_message )
	return(global.error_message);
    
    info->pre->next = info->next;
    info->next->pre = info->pre;
    XtFree(info->property_name);
    XtFree(info->protocol_name);
    XtFree(info);
    return(NULL);
}
