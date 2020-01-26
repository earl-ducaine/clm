static char sccsid[] = "@(#)TranslMgr.c	1.6 1/28/92";

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

#include "interface.h"
#include "functions.h"

ClmCommand *ClmFaugmentTranslations(cmd)
ClmCommand *cmd;
{
    Widget         widget;
    char          *msg;
    WidgetClass    class;
    XtTranslations actions;

    if( cmd->command != ClmAugmentTranslations || cmd->num_arg != 2 )
	GenError("ClmAugmentTranslations: illegal command record");

    if( msg = LookupWidget(IntArg0(cmd), &widget, &class, NULL) )
	GenWarn(msg);
    
    if( (actions = XtParseTranslationTable(StringArg1(cmd))) 
	 == NULL )
	GenWarn("Illegal translation table");

    ClmErrorTest();
    XtAugmentTranslations(widget,actions);
    ClmErrorTest();
    return(NULL);
}

ClmCommand *ClmFoverrideTranslations(cmd)
ClmCommand *cmd;
{
    Widget         widget;
    char          *msg;
    XtTranslations actions;

    if( cmd->command != ClmOverrideTranslations || cmd->num_arg != 2 )
	GenError("ClmOverrideTranslations: illegal command record");

    if( msg = LookupWidget(IntArg0(cmd), &widget, NULL, NULL) )
	GenWarn(msg);
    
    if( (actions = XtParseTranslationTable(StringArg1(cmd))) == NULL )
	GenWarn("Illegal translation table");

    ClmErrorTest();
    XtOverrideTranslations(widget,actions);
    ClmErrorTest();
    return(NULL);
}

ClmCommand *ClmFaddTabGroup(cmd)
ClmCommand *cmd;
{
    Widget         widget;
    char          *msg;

    if( cmd->command != ClmAddTabGroup || cmd->num_arg != 1 )
	GenError("ClmAddTabGroup: illegal command record");

    if( msg = LookupWidget(IntArg0(cmd), &widget, NULL, NULL) )
	GenWarn(msg);
    
    XmAddTabGroup(widget);
    return(NULL);
}

ClmCommand *ClmFremoveTabGroup(cmd)
ClmCommand *cmd;
{
    Widget         widget;
    char          *msg;

    if( cmd->command != ClmRemoveTabGroup || cmd->num_arg != 1 )
	GenError("ClmRemoveTabGroup: illegal command record");

    if( msg = LookupWidget(IntArg0(cmd), &widget, NULL, NULL) )
	GenWarn(msg);
    
    XmRemoveTabGroup(widget);
    return(NULL);
}

char *PushTranslations(id)
WidgetID id;
{
    static XtTranslations  empty_table = NULL;
    char                  *msg;
    Widget                 widget;
    WidgetNode            *node;
    Arg                    args[2];

    if( empty_table == NULL )
	empty_table = XtParseTranslationTable("");

    if( msg = LookupWidget(id, &widget, NULL, &node) )
	return(msg);
    
    if(node->translations)
	return("Translations already pushed");

    XtSetArg(args[0], XmNtranslations, &node->translations);
    XtGetValues(widget, args, 1);
    XtSetArg(args[0], XmNtranslations, empty_table);
    XtSetValues(widget, args, 1);
    return(NULL);
}

ClmCommand *ClmFpushTranslations(cmd)
ClmCommand *cmd;
{
    char *msg;
    register int i;

    if( cmd->command != ClmPushTranslations )
        GenError("ClmPushTranslations: Illegal command record");
    for(i=0; i<cmd->num_arg; i++) {
        if( msg = PushTranslations(IntArgn(cmd,i)) )
	    GenWarn(msg);
    }
    return(NULL);
}

char *PopTranslations(id)
WidgetID id;
{
    char                  *msg;
    Widget                 widget;
    WidgetNode            *node;
    Arg                    args[2];

    if( msg = LookupWidget(id, &widget, NULL, &node) )
	return(msg);

    if( ! node->translations)
	return("Translations already pushed");

    XtSetArg(args[0], XmNtranslations, node->translations);
    XtSetValues(widget, args, 1);
    node->translations = NULL;
    return(NULL);
}

ClmCommand *ClmFpopTranslations(cmd)
ClmCommand *cmd;
{
    char *msg;
    register int i;

    if( cmd->command != ClmPopTranslations )
        GenError("ClmPopTranslations: Illegal command record");
    for(i=0; i<cmd->num_arg; i++) {
        if( msg = PopTranslations(IntArgn(cmd,i)) )
	    GenWarn(msg);
    }
    return(NULL);
}
