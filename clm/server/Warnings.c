static char sccsid[] = "@(#)Warnings.c	1.6 1/28/92";

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
/*
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
*/

#include <Xm/PushB.h>

#include "interface.h"
#include "functions.h"
#include "opcodes.h"

static int terminated = TRUE;

static int loop_until_terminated()
{
    XEvent event;
    
    for(terminated = FALSE; terminated == FALSE; ) {
	XtNextEvent(&event);
	XtDispatchEvent(&event);
    }
}

static void do_terminate(widget, client_data, call_data)
Widget widget;
caddr_t client_data, call_data;
{
    terminated = TRUE;
}

ClmCommand *do_error(msg)
char *msg;
{
    SendError(global.socket, msg, ClmEventError);
    global.confirmed = True;
}

ClmCommand *do_warn(msg)
char *msg;
{
    SendError(global.socket, msg, ClmEventWarning);
    global.confirmed = True;
}


