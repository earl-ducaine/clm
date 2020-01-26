static char sccsid[] = "@(#)Resource.c	1.7 11/10/92";

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
#include <Xm/Text.h>
#include <Xm/DialogSP.h>

#include "interface.h"
#include "functions.h"

#define ArgTypeToString(type) (type ==  ClmArgInteger ? "Integer" : \
			      (type == ClmArgString ? "String" : \
			      (type == ClmArgSymbol ? "Symbol" : "Unknown")))

caddr_t ClmCvtToToolkit(class_id, parent_class_id, implicit_class_id, 
			external_name, value, 
			arg_type, resource_name, msg_ptr)
int       class_id, parent_class_id, implicit_class_id;
char     *external_name;
caddr_t  *value;
int       arg_type;
char    **resource_name;
char    **msg_ptr;
{
    register int               left, right, m;
    int                        in_constraints = 0, in_implicit = 0;
    register ClmResourceType  *tt;
    register ClmResourceDesc  *rt;
    register int               ts;
    int                        sign();
    static char                rmsg[200];

    rt = ClassTable[class_id].all_resources;
    ts = ClassTable[class_id].total_resources;

    /*  lookup resource in widget's resource table. If the resource can't be 
     *  found, lookup the widgets parents constraint table 
     */

lookup_constraints:

    left = 0;
    right = ts-1;

    while(left <= right) {
        m = (left+right)/2;
	switch(sign(strcmp(rt[m].external_name,external_name))) {
	    case 0:
		tt = TypeTable + rt[m].representation;
		if( rt[m].representation < 0 || tt->ToolkitRep == NULL ) {
		    sprintf(rmsg, "Resource <%s> currently not supported",
				  external_name);
		    *msg_ptr = rmsg;
		    return(NULL);
		}
		if( tt->arg_type != arg_type ) {
		    sprintf(rmsg, "Resource <%s> has illegal type for class %s.\n Now: %s Required: %s",
				  external_name, ClassTable[class_id].name,
				  ArgTypeToString(arg_type),
				  ArgTypeToString(tt->arg_type));
		    *msg_ptr = rmsg;
		    return(NULL);
		}
		*resource_name = rt[m].resource_name;
		*msg_ptr = NULL;
                return((*(tt->ToolkitRep))(*value, msg_ptr));
	    case -1:
		left = m+1;
		break;
	    case 1:
		right = m-1;
		break;
	}
    }

    if( ! in_constraints && parent_class_id != -1 ) {
	in_constraints++;
	rt = ClassTable[parent_class_id].all_constraints;
	ts = ClassTable[parent_class_id].total_constraints;
	goto lookup_constraints;
    }

    if( ! in_implicit && implicit_class_id != -1 ) {
        in_implicit++;
        rt = ClassTable[implicit_class_id].all_resources;
        ts = ClassTable[implicit_class_id].total_resources;
        goto lookup_constraints;
    }

    sprintf(rmsg,"Resource name <%s> not found for class %s(%d)", 
	    external_name, ClassTable[class_id].name, parent_class_id);
    *msg_ptr=rmsg;
    return(NULL);
}

void ClmCvtToExternal( class_id, parent_class_id, external_name, widget, 
		     type_ptr, value_ptr, msg_ptr )
int       class_id, parent_class_id;
char     *external_name;
Widget    widget;
int      *type_ptr;
caddr_t  *value_ptr;
char    **msg_ptr;
{
    int               left, right, m, sign(), in_constraints = 0;
    ClmResourceType  *tt;
    ClmResourceDesc  *rt;
    int               ts;
    static char       rmsg[200];

    rt = ClassTable[class_id].all_resources;
    ts = ClassTable[class_id].total_resources;

    /*  lookup resource in widget's resource table. If the resource can't be 
     *  found, lookup the widgets parents constraint table 
     */

lookup_constraints:

    left=0;
    right=ts-1;

    while(left<=right) {
        m=(left+right)/2;
	switch(sign(strcmp(rt[m].external_name,external_name))) {
	    case 0:
		tt = TypeTable + rt[m].representation;
		if( tt->ExternalRep == NULL ) {
		    *msg_ptr = "Resource currently not supported";
		    return;
		}
                (*(tt->ExternalRep))(rt[m].resource_name, widget, type_ptr,
				   value_ptr, msg_ptr);
		return;
	    case -1:
		left=m+1;
		break;
	    case 1:
		right=m-1;
		break;
	}
    }
    if( ! in_constraints && parent_class_id != -1 ) {
	in_constraints++;
	rt = ClassTable[parent_class_id].all_constraints;
	ts = ClassTable[parent_class_id].total_constraints;
	goto lookup_constraints;
    }
    sprintf(rmsg,"Resource name <%s> not found for class %s", 
	    external_name, ClassTable[class_id].name );
    *msg_ptr=rmsg;
    return;
}
