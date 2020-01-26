static char sccsid[] = "@(#)ResourceSet.c	1.7 2/24/92";

/*
 * Copyright 1989, 1990, 1991 GMD 
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
#include <sys/types.h>

#include <Xm/Xm.h>
#include <Xm/XmP.h>
#include <X11/StringDefs.h>
#include <X11/Vendor.h>

#ifdef GRAPHWIDGET
#include <Xbab/Graph.h>
#endif GRAPHWIDGET

#include "interface.h"
#include "functions.h"

caddr_t ClmPCvtAtom(value, msg_ptr)
caddr_t value;
char **msg_ptr;
{
    return((caddr_t)XmInternAtom(global.display, (char *)value, False));
}


caddr_t ClmPCvtDimension(value, msg_ptr)
caddr_t value;
char **msg_ptr;
{
    return(value);
}

caddr_t ClmPCvtTextPosition(value, msg_ptr)
caddr_t value;
char **msg_ptr;
{
    return(value);
}

caddr_t ClmPCvtInteger(value, msg_ptr)
caddr_t value;
char **msg_ptr;
{
    return(value);
}

caddr_t ClmPCvtPosition(value, msg_ptr)
caddr_t value;
char **msg_ptr;
{
    return(value);
}

caddr_t ClmPCvtShort(value, msg_ptr)
caddr_t value;
char **msg_ptr;
{
    return(value);
}

caddr_t ClmPCvtChar(value, msg_ptr)
caddr_t value;
char **msg_ptr;
{
    char *str= (char *)value;
	
    if( strlen(str) != 1 ) {
	 *msg_ptr = "string of length 1 expected";
	 return(NULL);
    }
    return((caddr_t)(*str));
}

caddr_t ClmPCvtUChar(value, msg_ptr)
caddr_t value;
char **msg_ptr;
{
    unsigned char *str= (unsigned char *)value;
	
    if( strlen((char*)str) != 1 ) {
	 *msg_ptr = "string of length 1 expected";
	 return(NULL);
    }
    return((caddr_t)(*str));
}

caddr_t ClmPCvtString(value, msg_ptr)
caddr_t value;
char **msg_ptr;
{
    char *str= (char *)value;
    char *s;

    s = ClmMalloc(strlen(str)+1);
    strcpy(s, str);
    return(s);
}

caddr_t ClmPCvtFontList(value, msg_ptr)
caddr_t value;
char **msg_ptr;
{
    char *fontname = (char *)value;
    XFontStruct *font;
    XmStringCharSet cset = (XmStringCharSet) XmSTRING_DEFAULT_CHARSET;

    if( (font = XLoadQueryFont(global.display, fontname) ) == NULL ) {
	*msg_ptr = "Can't load font";
	return(NULL);
    }
    return((caddr_t)XmFontListCreate(font, cset));
}

caddr_t ClmPCvtBoolean(value, msg_ptr)
caddr_t value;
char **msg_ptr;
{
    if( ! strcmp(value, "T") )
      return((caddr_t)TRUE);

    if( ! strcmp(value, "NIL") )
      return((caddr_t)FALSE);

    *msg_ptr = "Boolean value must be t or nil";
    return((caddr_t)NULL);
}

caddr_t ClmPCvtXmString(value, msg_ptr)
caddr_t value;
char **msg_ptr;
{
    XmString s, XmStringCreate();

    if( (s = XmStringCreateLtoR((char *)value,
				XmSTRING_DEFAULT_CHARSET)) == NULL ) {
        *msg_ptr = "String to XmString conversion failed";
        return((caddr_t)NULL);
    }
    return((caddr_t)s);
}

Status ClmParseColor(color_string, color_ptr)
char *color_string;
XColor *color_ptr;
{
    XColor    exact_color;
    Screen    *screen = XDefaultScreenOfDisplay(global.display);
    Colormap  colormap = XDefaultColormapOfScreen(screen);
    Status    status;

    if( ! strcmp(color_string, "") ) {
	return(0);
    }
    /* Convert a RGB specification into an X Color */
    if( color_string[0] == '#' ) {
	status = XParseColor(global.display, colormap, color_string, color_ptr);
	if( status != 0 )
	    status = XAllocColor(global.display, colormap, color_ptr);
    }
    else {
	/* Convert a named color (i.e. "red") into an X color */
	status = XAllocNamedColor(global.display, colormap, color_string,
			          color_ptr, &exact_color);
    }
    return(status);
}

caddr_t ClmPCvtPixel(value, msg_ptr)
caddr_t value;
char **msg_ptr;
{
    char  *color_name = (char *)value;
    XColor color;
    Pixel  pixel;

    if( color_name[0] == '%' ) {
	sscanf(color_name+1, "%d", &pixel);
	return((caddr_t)pixel);
    }
    else if( ClmParseColor(color_name, &color) == 0 ) {
        *msg_ptr = "Can't parse color specification";
	return((caddr_t)NULL);
    }
    else
	/* Return the Pixel value */
	return((caddr_t)color.pixel);
}

caddr_t ClmPCvtPixmap(value, msg_ptr)
caddr_t value;
char **msg_ptr;
{
    Pixmap        pixmap;
    char         *image_name = (char *)value;
    Screen       *screen = DefaultScreenOfDisplay(global.display);
    Pixel         background, foreground;
    Arg           args[1];

    if( ! strcmp(image_name, "") )
	return((caddr_t)XmUNSPECIFIED_PIXMAP);

    if( image_name[0] == '%' ) {
	sscanf(image_name+1, "%d", &pixmap);
	return((caddr_t)pixmap);
    } else {
	if( global.app_shell_ptr ) {
	    XtSetArg(args[0], XmNbackground, &background);
	    XtGetValues(global.app_shell_ptr, args, 1);
	} else {
	    background = XWhitePixelOfScreen(screen);
	}
#if XmREVISION != 0
	XmGetColors(screen, DefaultColormapOfScreen(screen), background,
		    &foreground, NULL, NULL, NULL);
#else
        foreground = XBlackPixelOfScreen(screen);
#endif
	return((caddr_t)XmGetPixmap(screen, image_name, foreground, background));
    }
}

caddr_t ClmPCvtWidget(value, msg_ptr)
caddr_t value;
char **msg_ptr;
{
    Widget       widget;
    char        *msg;
    WidgetClass  class;

    if( msg = LookupWidget((WidgetID)value, &widget, &class, NULL) )
	*msg_ptr = msg;
    return((caddr_t)widget);
}

caddr_t ClmPCvtTrans(value, msg_ptr)
caddr_t value;
char **msg_ptr;
{
    XtTranslations actions;

    if( ( actions = XtParseTranslationTable((char *)value) ) == NULL )
	*msg_ptr = "XtParseTranslationTable failed";
    return((caddr_t)actions);
}

caddr_t ClmPCvtFloat(value, msg_ptr)
caddr_t value;
char    **msg_ptr;
{
    return(value);
}

caddr_t ClmPCvtKeysym(value, msg_ptr)
caddr_t value;
char **msg_ptr;
{
    char *s;
    KeySym k;

    s = (char *) value;

    k = XStringToKeysym(s);

    return((caddr_t) k);
}
