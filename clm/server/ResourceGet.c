static char sccsid[] = "@(#)ResourceGet.c	1.7 1/30/92";

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

#include <X11/Xlib.h>
#include <X11/IntrinsicP.h>
#include <X11/StringDefs.h>
#include <X11/Vendor.h>
#include <Xm/DialogSP.h>
#include <Xm/XmP.h>

#include "interface.h"
#include "functions.h"

void ClmTCvtXmString(name, widget, type_ptr, value_ptr, msg_ptr)
char     *name;
Widget    widget;
int      *type_ptr;
caddr_t  *value_ptr;
char    **msg_ptr;
{
    Arg arg[1];
    XmString xms;
    char     *s;

    XtSetArg(arg[0], name, &xms);
    XtGetValues(widget, arg, 1);
    if( XmStringGetLtoR(xms, XmSTRING_DEFAULT_CHARSET, &s)) {
	*type_ptr  = ClmArgString;
	*value_ptr = (caddr_t)s;
    }
    else {
	*msg_ptr = "XmStringGetLtoR failed";
    }
    return;
}

void ClmTCvtWidget(name, widget, type_ptr, value_ptr, msg_ptr)
char     *name;
Widget    widget;
int      *type_ptr;
caddr_t  *value_ptr;
char    **msg_ptr;
{
    Arg         arg[1];
    Widget      w; 
    WidgetID    widget_id;
    int         class, parent_class;

    XtSetArg(arg[0], name, &w);
    XtGetValues(widget, arg, 1);


    if( w == NULL ) {
	/* A nonexistant widget is not an error */
	*type_ptr  = ClmArgInteger;
	*value_ptr = (caddr_t)(-1);
	return;
    } else {
	/* Widget exists. Insert it into WidgetTable if necessary */
	widget_id = EncapsulateWidget (w, EW_GENERATE_ID_IF_NECESSARY,
				       EW_COMPUTE_CLASS_ID, EW_COMPUTE_CLASS_ID,
				       msg_ptr);
	if (widget_id < 0) 
	    return;

	*type_ptr  = ClmArgInteger;
	*value_ptr = (caddr_t)widget_id;
    }
}

void ClmTCvtDimension(name, widget, type_ptr, value_ptr, msg_ptr)
char     *name;
Widget    widget;
int      *type_ptr;
caddr_t  *value_ptr;
char    **msg_ptr;
{
    Arg arg[1];
    Dimension d;

    *type_ptr = ClmArgInteger;
    XtSetArg(arg[0], name, &d);
    XtGetValues(widget, arg, 1);
    *value_ptr = (caddr_t)d;
    return;
}

void ClmTCvtTextPosition(name, widget, type_ptr, value_ptr, msg_ptr)
char     *name;
Widget    widget;
int      *type_ptr;
caddr_t  *value_ptr;
char    **msg_ptr;
{
    Arg arg[1];
#if XmREVISION != 0
    XmTextPosition d;
#else
    int d;
#endif

    *type_ptr = ClmArgInteger;
    XtSetArg(arg[0], name, &d);
    XtGetValues(widget, arg, 1);
    *value_ptr = (caddr_t)d;
    return;
}

void ClmTCvtInteger(name, widget, type_ptr, value_ptr, msg_ptr)
char     *name;
Widget    widget;
int      *type_ptr;
caddr_t  *value_ptr;
char    **msg_ptr;
{
    Arg arg[1];
    int i;

    *type_ptr = ClmArgInteger;
    XtSetArg(arg[0], name, &i);
    XtGetValues(widget, arg, 1);
    *value_ptr = (caddr_t) i;
    return;
}

void ClmTCvtPosition(name, widget, type_ptr, value_ptr, msg_ptr)
char     *name;
Widget    widget;
int      *type_ptr;
caddr_t  *value_ptr;
char    **msg_ptr;
{
    Arg arg[1];
    Position p;

    *type_ptr = ClmArgInteger;
    XtSetArg(arg[0], name, &p);
    XtGetValues(widget, arg, 1);
    *value_ptr = (caddr_t) p;
    return;
}

void ClmTCvtShort(name, widget, type_ptr, value_ptr, msg_ptr)
char     *name;
Widget    widget;
int      *type_ptr;
caddr_t  *value_ptr;
char    **msg_ptr;
{
    Arg   arg[1];
    short s;

    *type_ptr = ClmArgInteger;
    XtSetArg(arg[0], name, &s);
    XtGetValues(widget, arg, 1);
    *value_ptr = (caddr_t)s;
    return;
}

void ClmTCvtUChar(name, widget, type_ptr, value_ptr, msg_ptr)
char     *name;
Widget    widget;
int      *type_ptr;
caddr_t  *value_ptr;
char    **msg_ptr;
{
    Arg   arg[1];
    unsigned char uc;
    unsigned char *s;

    s = (unsigned char *)ClmMalloc(2);
    *type_ptr = ClmArgString;
    XtSetArg(arg[0], name, &uc);
    XtGetValues(widget, arg, 1);
    s[0] = uc;
    s[1] = '\0';
    *value_ptr = (caddr_t)s;
    return;
}

void ClmTCvtString(name, widget, type_ptr, value_ptr, msg_ptr)
char     *name;
Widget    widget;
int      *type_ptr;
caddr_t  *value_ptr;
char    **msg_ptr;
{
    Arg arg[1];

    *type_ptr = ClmArgString;
    XtSetArg(arg[0], name, value_ptr);
    XtGetValues(widget, arg, 1);
    return;
}

void ClmTCvtBool(name, widget, type_ptr, value_ptr, msg_ptr)
char     *name;
Widget    widget;
int      *type_ptr;
caddr_t  *value_ptr;
char    **msg_ptr;
{
    Arg     arg[1];
    Bool    bool;

    *type_ptr = ClmArgSymbol;
    XtSetArg(arg[0], name, &bool);
    XtGetValues(widget, arg, 1);
    *value_ptr = (bool ? "T" : "NIL");
    return;
}

void ClmTCvtBoolean(name, widget, type_ptr, value_ptr, msg_ptr)
char     *name;
Widget    widget;
int      *type_ptr;
caddr_t  *value_ptr;
char    **msg_ptr;
{
    Arg     arg[1];
    Boolean bool;

    *type_ptr = ClmArgSymbol;
    XtSetArg(arg[0], name, &bool);
    XtGetValues(widget, arg, 1);
    *value_ptr = (bool ? "T" : "NIL");
    return;
}

void ClmTCvtPixel(name, widget, type_ptr, value_ptr, msg_ptr)
char     *name;
Widget    widget;
int      *type_ptr;
caddr_t  *value_ptr;
char    **msg_ptr;
{
    Arg   arg[1];
    Pixel pixel;

    *type_ptr = ClmArgInteger;
    XtSetArg(arg[0], name, &pixel);
    XtGetValues(widget, arg, 1);
    *value_ptr = (caddr_t)pixel;
    return;
}

void ClmTCvtPixmap(name, widget, type_ptr, value_ptr, msg_ptr)
char     *name;
Widget    widget;
int      *type_ptr;
caddr_t  *value_ptr;
char    **msg_ptr;
{
    Arg    arg[1];
    Pixmap pixmap;
    static char pixmap_name[20];
    
    *type_ptr = ClmArgString;
    XtSetArg(arg[0], name, &pixmap);
    XtGetValues(widget, arg, 1);
    if( pixmap == XmUNSPECIFIED_PIXMAP )
	strcpy(pixmap_name, "woman");
    else
	sprintf(pixmap_name, "%s%d", "%", pixmap);
    *value_ptr = (caddr_t)pixmap_name;
}

void ClmTCvtFloat(name, widget, type_ptr, value_ptr, msg_ptr)
char     *name;
Widget    widget;
int      *type_ptr;
float    *value_ptr;
char    **msg_ptr;
{
    Arg arg[1];
    float f;

    *type_ptr = ClmArgFloat;
    XtSetArg(arg[0], name, &f );
    XtGetValues(widget, arg, 1 );
    *value_ptr = f;
    return;
}

void ClmTCvtKeysym(name, widget, type_ptr, value_ptr, msg_ptr)
char     *name;
Widget    widget;
int      *type_ptr;
caddr_t  *value_ptr;
char    **msg_ptr;
{
    Arg   arg[1];
    KeySym k;
    char *s;

    *type_ptr = ClmArgString;
    XtSetArg(arg[0], name, &k);
    XtGetValues(widget, arg, 1);

    s = XKeysymToString(k);
    *value_ptr = (caddr_t)s;
}

void ClmTCvtCursor(name, widget, type_ptr, value_ptr, msg_ptr)
char     *name;
Widget    widget;
int      *type_ptr;
caddr_t  *value_ptr;
char    **msg_ptr;
{
    Arg arg[1];
    Cursor x;

    XtSetArg(arg[0], name, &x);
    XtGetValues(widget, arg, 1);

    *type_ptr = ClmArgInteger;
    *value_ptr = (caddr_t) x;
    return;
}

void ClmTCvtFont(name, widget, type_ptr, value_ptr, msg_ptr)
char     *name;
Widget    widget;
int      *type_ptr;
caddr_t  *value_ptr;
char    **msg_ptr;
{
    Arg arg[1];
    Font x;

    XtSetArg(arg[0], name, &x);
    XtGetValues(widget, arg, 1);

    *type_ptr = ClmArgInteger;
    *value_ptr = (caddr_t) x;
    return;
}

void ClmTCvtColormap(name, widget, type_ptr, value_ptr, msg_ptr)
char     *name;
Widget    widget;
int      *type_ptr;
caddr_t  *value_ptr;
char    **msg_ptr;
{
    Arg arg[1];
    Colormap x;

    XtSetArg(arg[0], name, &x);
    XtGetValues(widget, arg, 1);

    *type_ptr = ClmArgInteger;
    *value_ptr = (caddr_t) x;
    return;
}

void ClmTCvtAtom(name, widget, type_ptr, value_ptr, msg_ptr)
char     *name;
Widget    widget;
int      *type_ptr;
caddr_t  *value_ptr;
char    **msg_ptr;
{
    Arg arg[1];
    Atom x;

    XtSetArg(arg[0], name, &x);
    XtGetValues(widget, arg, 1);

    *type_ptr = ClmArgInteger;
    *value_ptr = (caddr_t) x;
    return;
}

void ClmTCvtScreen(name, widget, type_ptr, value_ptr, msg_ptr)
char     *name;
Widget    widget;
int      *type_ptr;
caddr_t  *value_ptr;
char    **msg_ptr;
{
    Arg arg[1];
    Screen *x;

    XtSetArg(arg[0], name, &x);
    XtGetValues(widget, arg, 1);

    *type_ptr = ClmArgInteger;
    *value_ptr = (caddr_t) XScreenNumberOfScreen(x);
    return;
}

void ClmTCvtVisual(name, widget, type_ptr, value_ptr, msg_ptr)
char     *name;
Widget    widget;
int      *type_ptr;
caddr_t  *value_ptr;
char    **msg_ptr;
{
    Arg arg[1];
    Visual *x;

    XtSetArg(arg[0], name, &x);
    XtGetValues(widget, arg, 1);

    *type_ptr = ClmArgInteger;
    *value_ptr = (caddr_t) (x ? XVisualIDFromVisual(x) : 0);
    return;
}
