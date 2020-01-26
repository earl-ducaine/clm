static char sccsid[] = "@(#)GinaView.c	1.6 1/28/92";

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
#include <Xm/DrawingAP.h>

#include "interface.h"
#include "functions.h"

typedef struct _XtEventRec {
     XtEventTable       next;
     EventMask          mask;
     Boolean            non_filter;
     Boolean            select;
     Boolean            raw;
     Boolean            async;
     XtEventHandler     proc;
     caddr_t            closure;
} XtEventRec;

void InitializeGinaView(view)
XmDrawingAreaWidget view;
{
    Bool done=False;
    XtEventTable table;

    while( ! done ) {
	done = True;
        for(table = view->core.event_table; table; table = table->next) {
	    if( table->mask & ButtonPressMask ) {
		XtRemoveEventHandler(view, table->mask, table->non_filter,
				     table->proc, table->closure);

		done = False;
		break;
	    }
	}
    }
}
