static char sccsid[] = "@(#)ProtocolMgr.c	1.6 1/28/92";

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

ClmCommand *ClmFprotocolAddCallback(cmd)
ClmCommand *cmd;
{
    Widget            widget;
    char             *msg;

    if( cmd->command != ClmProtocolAddCallback || cmd->num_arg != 3 ) 
	GenError("ClmProtocolAddCallback: illegal command record");

    if( msg = LookupWidget(IntArg0(cmd), &widget, NULL ,NULL) )
	GenWarn(msg);
    
    if( msg = ClmProtocolCallbackAdd(widget, IntArg0(cmd),
				     SymbolArg1(cmd), SymbolArg2(cmd)))
	GenWarn(msg);

    return(NULL);
}

ClmCommand *ClmFprotocolRemoveCallback(cmd)
ClmCommand *cmd;
{
    Widget       widget;
    char        *msg;

    if( cmd->command != ClmProtocolRemoveCallback || cmd->num_arg != 3 )
	GenError("ClmProtocolRemoveCallback: illegal command record");

    if( msg = LookupWidget(IntArg0(cmd), &widget, NULL, NULL) )
	GenWarn(msg);
    
    if( msg = ClmProtocolCallbackRemove(widget, IntArg0(cmd),
				        SymbolArg1(cmd), SymbolArg2(cmd)))
	GenWarn(msg);

    return(NULL);
}
