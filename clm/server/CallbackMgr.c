static char sccsid[] = "@(#)CallbackMgr.c	1.7 1/28/92";

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

ClmCommand *ClmFaddCallback(cmd)
ClmCommand *cmd;
{
    Widget            widget;
    char             *msg;

    if( cmd->command != ClmAddCallback || cmd->num_arg != 2 ) 
	GenError("ClmAddCallback: illegal command record");

    if( msg = LookupWidget(IntArg0(cmd), &widget, NULL ,NULL) )
	GenWarn(msg);
    
    if( msg = ClmCallbackAdd(widget, IntArg0(cmd), SymbolArg1(cmd)))
	GenWarn(msg);

    return(NULL);
}

ClmCommand *ClmFremoveCallback(cmd)
ClmCommand *cmd;
{
    Widget       widget;
    char        *msg;

    if( cmd->command != ClmRemoveCallback || cmd->num_arg != 2 )
	GenError("ClmRemoveCallback: illegal command record");

    if( msg = LookupWidget(IntArg0(cmd), &widget, NULL, NULL) )
	GenWarn(msg);
    
    if( msg = ClmCallbackRemove(widget, IntArg0(cmd), SymbolArg1(cmd)))
	GenWarn(msg);

    return(NULL);
}
