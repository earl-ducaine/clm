static char sccsid[] = "@(#)Classes.c	1.7 1/30/92";

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
#include <stdlib.h>

#include CLASS_TABLE_MODULE

char *ExternalName(name)
register char *name;
{
    register char *s = name, *external_name;
    int length=0;

    while(*s) {
	if( isupper(*s) && s != name )
	    length ++;
	length++;
	s++;
    }

    if( (external_name = (char *)malloc(length+1)) == NULL ) {
	perror("malloc");
	exit(1);
    }

    s = external_name;
    while(*name) {
        /* convert keyboardFocusType --> KEYBOARD-FOCUS-TYPE */
	if( islower(*name) ) {
	    *s = toupper(*name);
	}
	else {
	    if( isupper(*name) && s != external_name ) {
		*s = '-';
		++s;
	    }
	    *s = *name;
	}
	s++;
	name++;
    }
    *s = '\0';
    return(external_name);
}

int RepIndex(resource_type)
char *resource_type;
{
    int i;

    for(i=0; TypeTable[i].resource_type != NULL; i++ )
	if( ! strcmp(TypeTable[i].resource_type, resource_type) )
	    return(i);
    return(-1);
}

char *InitializeClassTable()
{
    int i;

    for( i=0; i<num_classes; i++)
	ClassTable[i].class =
	  (WidgetClass)(*(WidgetClass *)(ClassTable[i].class));
}

int ClassPointerToIndex(class)
register WidgetClass class;
{
    register int i;

    for( i=0; i<num_classes; i++ )
	if( ClassTable[i].class == class )
	    return(i);
    fprintf (stderr, "Implementation error: widget class not in table.");
    fflush (stderr);
    abort();
}

int ClassNameToIndex(name, msg_ptr)
register char *name;
         char **msg_ptr;
{
    register int  i;
    static   char msg[100];

    for( i=0; i<num_classes; i++ )
	if( ClassTable[i].name && ! strcmp(ClassTable[i].name, name) )
	    return(i);
    sprintf(msg,"Class %s not found",name);
    *msg_ptr = msg;
    return(-1);
}
