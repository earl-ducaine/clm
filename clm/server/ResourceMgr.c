static char sccsid[] = "@(#)ResourceMgr.c	1.6 1/28/92";

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

Arg *ClmMakeArgList(class_id, parent_class_id, implicit_class_id, 
                    list, num_arg, msg_ptr,
		    ret_args, managed_ptr)
int      class_id;
int      parent_class_id;
int      implicit_class_id;   /* for convenience combinations */
ClmArg  *list;
int      num_arg;
char   **msg_ptr;
int     *ret_args;
Boolean *managed_ptr;
{
    Arg     *args;
    char    *toolkit_name;
    int      i; 
    caddr_t  value;

    args = (Arg *)ClmMalloc(sizeof(Arg)*(num_arg == 0 ? 1 : num_arg));

    for( *ret_args = 0, i = 0; i < num_arg; i += 2) {
        if(list[i].arg_type != ClmArgSymbol ) {
	    *msg_ptr = "Resource name must be an Atom";
	    return(NULL);
	}
	if( managed_ptr != NULL && list[i+1].arg_type == ClmArgSymbol &&
	    ! strcmp( "MANAGED", (char *)list[i].v.symbol_value) ) {
	    if( ! strcmp( "NIL", (char *)list[i+1].v.symbol_value) )
		*managed_ptr = False;
	    else 
		*managed_ptr = True;
	} else {
	    value = ClmCvtToToolkit(class_id, parent_class_id, 
				    implicit_class_id,
				    (char *)list[i].v.symbol_value, 
				    &(list[i+1].v), 
				    list[i+1].arg_type,
				    &toolkit_name,
				    msg_ptr);
	    if( *msg_ptr )
		return(NULL);
	    XtSetArg(args[*ret_args], toolkit_name, value);
	    (*ret_args)++;
	}
    }
    return(args);
}

ClmCommand *ClmFgetValues(cmd)
ClmCommand *cmd;
{
    Widget             widget;
    static ClmCommand  command;
    static ClmArg     *arg = NULL;
    int                arg_type;
    int                value;
    int                i;
    char              *msg;
    int                class_id, parent_class_id;

    if( cmd->command != ClmGetValues || cmd->num_arg < 1 )
	GenError("ClmGetValues: illegal command record");

    if( msg = LookupWidget(IntArg0(cmd),&widget,&class_id, NULL) )
        GenWarn(msg);
    if( msg = LookupClassOfParentOfWidget(IntArg0(cmd), &parent_class_id) )
        GenWarn(msg);

    if( arg )
	free(arg);

    arg = (ClmArg *)ClmMalloc(sizeof(ClmArg)*cmd->num_arg-1);

    command.command = ClmReturnValues;
    command.num_arg = cmd->num_arg-1;
    command.args    = arg;

    for( i=1; i<cmd->num_arg; ++i ) {
	if( cmd->args[i].arg_type != ClmArgSymbol )
	    GenWarn("Resource name must be an atom");
	msg = NULL;
	ClmCvtToExternal(class_id, parent_class_id, SymbolArgn(cmd,i),
		         widget, &arg_type, &value, &msg );
	command.args[i-1].arg_type = arg_type;
	command.args[i-1].v.int_value    = value;
	if( msg )
	    GenWarn(msg);
    }
    return(&command);
}
	
ClmCommand *ClmFsetValues(cmd)
ClmCommand *cmd;
{
    int     class_id, parent_class_id, ret_args;
    char   *msg;
    Arg    *args;
    Widget  widget;

    if( cmd->command != ClmSetValues || cmd->num_arg == 0 ||
	cmd->num_arg % 2 == 0 )
	GenError("ClmSetValues: illegal command record");
  
  
    if( msg = LookupWidget(IntArg0(cmd), &widget, &class_id, NULL) )
        GenWarn(msg);
    if( ( msg = LookupClassOfParentOfWidget(IntArg0(cmd),
					    &parent_class_id) ) != NULL )
        GenWarn(msg);

    global.display = XtDisplay(widget);
    if( (args = ClmMakeArgList(class_id, parent_class_id, -1, (cmd->args)+1,
			       cmd->num_arg-1, &msg, &ret_args, NULL)) == NULL ) 
	GenWarn(msg);

    XtSetValues(widget, args, ret_args);
    if( global.forced_output )
	XmUpdateDisplay(widget);
    free(args);
    ClmErrorTest();
    return(NULL);
}

/*****************************************************************************
 *  Obtain a description of the widget's resources
 *  Return's a list with 3*total_resources elements, each tripel
 *  contains name, type an default value of the resource
 ****************************************************************************/

ClmCommand *ClmFgetResources(cmd)
ClmCommand *cmd;
{
    return(NULL);
}
