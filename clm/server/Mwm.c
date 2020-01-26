static char sccsid[] = "@(#)Mwm.c	1.7 9/8/93";

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

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <Xm/Xm.h>

#include "interface.h"
#include "functions.h"

static ClmArg     MwmA[1];
static ClmCommand MwmC = {ClmReturnValues, -1, 1, MwmA};

ClmCommand *ClmFisMwmRunning(cmd)
ClmCommand *cmd;
{
    Widget widget;
    char  *msg;

    if(cmd->command != ClmIsMwmRunning || cmd->num_arg != 1)
	GenError("is-mwm-running: illegal command record");

    if( msg = LookupWidget(cmd->args[0].v.int_value, &widget, NULL, NULL) )
	GenWarn(msg);

    MwmA[0].arg_type = ClmArgSymbol;
    MwmA[0].v.symbol_value = XmIsMotifWMRunning(widget) ? "T" : "NIL";

    return(&MwmC);
}
