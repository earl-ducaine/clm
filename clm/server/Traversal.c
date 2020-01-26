static char sccsid[] = "@(#)Traversal.c	1.3 1/28/92";

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

#include <ctype.h>
#include <stdio.h>

#include <Xm/Xm.h>

#include "interface.h"
#include "functions.h"

static ClmArg     TraversalArgs[1];
static ClmCommand TraversalCmd = {ClmReturnValues, -1, 1, TraversalArgs};

ClmCommand *ClmFprocessTraversal(cmd)
ClmCommand *cmd;
{
    Widget          widget;
    char           *msg;
    char           *sym;
    int    direction;

    if(cmd->command != ClmProcessTraversal || cmd->num_arg != 2)
	GenError("ClmProcessTraversal: illegal command record");

    if( msg = LookupWidget(IntArg0(cmd), &widget, NULL, NULL) )
	GenWarn(msg);

#if XmREVISION != 0
    sym = SymbolArg1(cmd);
    if( ! strcmp(sym, "CURRENT") || 
        ! strcmp(sym, "TRAVERSE-CURRENT") )
	direction = XmTRAVERSE_CURRENT;
    else if( ! strcmp(sym, "NEXT") ||
	     ! strcmp(sym, "TRAVERSE-NEXT") )
	direction = XmTRAVERSE_NEXT;
    else if( ! strcmp(sym, "RIGHT") ||
	     ! strcmp(sym, "TRAVERSE-RIGHT") )
	direction = XmTRAVERSE_RIGHT;
    else if( ! strcmp(sym, "DOWN") ||
	     ! strcmp(sym, "TRAVERSE-DOWN") )
	direction = XmTRAVERSE_DOWN;
    else if( ! strcmp(sym, "LEFT") ||
	     ! strcmp(sym, "TRAVERSE-LEFT") )
	direction = XmTRAVERSE_LEFT;
    else if( ! strcmp(sym, "UP") ||
	     ! strcmp(sym, "TRAVERSE-UP") )
	direction = XmTRAVERSE_UP;
    else if( ! strcmp(sym, "HOME") ||
	     ! strcmp(sym, "TRAVERSE-HOME") )
	direction = XmTRAVERSE_HOME;
    else if( ! strcmp(sym, "NEXT-TAB-GROUP") || 
	     ! strcmp(sym, "TRAVERSE-NEXT-TAB-GROUP") )
	direction = XmTRAVERSE_NEXT_TAB_GROUP;
    else if( ! strcmp(sym, "PREV-TAB-GROUP") ||
	     ! strcmp(sym, "TRAVERSE-PREV-TAB-GROUP") )
	direction = XmTRAVERSE_PREV_TAB_GROUP;
    else 
#endif
	GenWarn("process-traversal: illegal direction keyword");

#if XmREVISION != 0
    TraversalArgs[0].arg_type = ClmArgInteger;
    TraversalArgs[0].v.int_value =
	XmProcessTraversal(widget, direction) ? 1 : 0;
    return(&TraversalCmd);
#endif
}




