static char sccsid[] = "@(#)Cursor.c	1.9 9/8/93";

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
 *
 *          Thomas Spandoeck
 *          VW-GEDAS
 *          Pascalstr. 11
 *          1000 Berlin 10
 */

#include <stdio.h>

#include <Xm/Xm.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>

#include "interface.h"
#include "functions.h"

#define PIXMAP_CURSOR 0
#define FONT_CURSOR 1

static ClmArg c_args[1];
static ClmCommand c_cmd = {ClmReturnValues, -1, 1, c_args};

static ClmArg q_args[4];
static ClmCommand q_cmd = {ClmReturnValues, -1, 4, q_args};

ClmCommand *ClmFqueryCursor(cmd)
ClmCommand *cmd;
{
    unsigned int mask;
    Window root, child;
    char        *msg;
    Widget widget;

    if(cmd->command != ClmQueryCursor || cmd->num_arg != 1)
	GenError("ClmFqueryCursor: illegal command record");

    if( msg = LookupWidget(IntArg0(cmd), &widget, NULL, NULL) )
        GenWarn(msg);

    q_args[0].arg_type = ClmArgInteger;
    q_args[1].arg_type = ClmArgInteger;
    q_args[2].arg_type = ClmArgInteger;
    q_args[3].arg_type = ClmArgInteger;
    XQueryPointer(XtDisplay(widget), XtWindow(widget), &root, &child,
	                    &q_args[0].v.int_value,
	                    &q_args[1].v.int_value,
	                    &q_args[2].v.int_value,
	                    &q_args[3].v.int_value, &mask);
	
    return(&q_cmd);
}

ClmCommand *ClmFchangeCursor(cmd)
ClmCommand *cmd;
{
    unsigned int mask;

    if(cmd->command != ClmChangeCursor || cmd->num_arg != 3)
	GenError("ChangeCursor illegal command record");

    mask = (ButtonReleaseMask | ButtonMotionMask | IntArg1(cmd) ) & 
	   ~ IntArg2(cmd);

    /*fprintf(stderr, "mask=%x cursor=%d \n", mask, IntArg0(cmd)); 
    fflush(stderr); */
    XChangeActivePointerGrab(global.display, mask, IntArg0(cmd),
#if XmREVISION != 0
			     XtLastTimestampProcessed(global.display));
#else
                             CurrentTime);
#endif

    return(NULL);
}

int ClmReadBitmapFile (name, width_return, height_return, bitmap_return,
                       x_hot_return, y_hot_return)
char         *name;
unsigned int *width_return, *height_return;
Pixmap       *bitmap_return;
int          *x_hot_return, *y_hot_return;
{
    char     *bm_path, *file_name;
    int      return_status;

#if XmREVISION != 0
    SubstitutionRec subs[1];

    subs[0].match = 'B';
    subs[0].substitution = name;

    if (name[0] == '/')
	bm_path = "%B";
    else if ((bm_path = (char *) getenv("XBMLANGPATH")) == NULL)
	bm_path = "/usr/lib/X11/bitmaps/%B";

    file_name = XtResolvePathname (global.display, "bitmaps",
				   NULL, NULL, bm_path,
				   subs, XtNumber(subs), NULL);
#else
    file_name = name;
#endif

    if (file_name) {
	return_status = XReadBitmapFile (global.display,
					 DefaultRootWindow(global.display),
					 file_name,
					 width_return, height_return,
					 bitmap_return,
					 x_hot_return, y_hot_return);
#if XmREVISION != 0
	XtFree (file_name);
#endif
	return return_status;
    }
    else
	return BitmapOpenFailed;
}

ClmCommand *ClmFcreateCursor(cmd)
ClmCommand *cmd;
{
    int        rbit_status;
    Pixmap     m_bitmap, s_bitmap;
    char      *color_string;
    Status     ClmParseColor();  /* From ResourceSet.c */
    Screen    *screen = XDefaultScreenOfDisplay(global.display);
    Colormap   colormap = XDefaultColormapOfScreen(screen);
    XColor     foreground, background, exact_color;
    Cursor     cursorid;
    unsigned int  
	       m_width, m_height, s_width, s_height;
    int        m_x_hot, m_y_hot, s_x_hot, s_y_hot;

    if(cmd->command != ClmCreatePixmapCursor)
      GenError("ClmFcreatePixmapCursor: illegal command record")
    else {
      switch (IntArg0(cmd)){
      case PIXMAP_CURSOR:
	if(cmd->num_arg != 5)
	  GenError("ClmFcreatePixmapCursor: illegal command record")
	else {
	  rbit_status =
	    ClmReadBitmapFile(StringArg2(cmd),       /*  mask-file */
			      &m_width, &m_height, &m_bitmap, 
			      &m_x_hot, &m_y_hot);
	  if(rbit_status != BitmapSuccess)
	    GenWarn("ClmFcreatePixmapCursor: XReadBitmapFile failed")
	  rbit_status =
	    ClmReadBitmapFile(StringArg1(cmd),       /*  source-file */
			      &s_width, &s_height, &s_bitmap, 
			      &s_x_hot, &s_y_hot);
	  if(rbit_status != BitmapSuccess)
	    GenWarn("ClmFcreatePixmapCursor: XReadBitmapFile failed")
	  /* Check for a valid hot spot */
          if( s_x_hot < 0 || s_y_hot < 0 || 
	      s_x_hot > s_width || s_y_hot > s_height )
	     GenWarn("Hot spot undefined or outside bitmap for source bitmap");
	  /* Check for equal sizes of source & mask */
	  if( s_width != m_width || s_height != m_height )
	      GenWarn("Source & mask have different width and/or height");

	  if( ClmParseColor(StringArg3(cmd), &foreground) == 0 )
	      GenWarn("Failed to parse foreground color");
	  
	  if( ClmParseColor(StringArgn(cmd, 4), &background) == 0 )
	      GenWarn("Failed to parse background color");
	  
	  cursorid = XCreatePixmapCursor(global.display,
					 s_bitmap, m_bitmap,
					 &foreground, &background,
					 (unsigned int)s_x_hot, 
					 (unsigned int)s_y_hot);

	}
	break;
      case FONT_CURSOR:
	if(cmd->num_arg != 2)
	  GenError("ClmFcreatePixmapCursor: illegal command record")
	else{
	  cursorid = XCreateFontCursor(global.display, IntArg1(cmd));
	}
	break;
      default:
	break;
      }
      c_args[0].arg_type = ClmArgInteger;
      c_args[0].v.int_value = cursorid;
      /*fprintf(stderr, "cursor=%d \n", cursorid); fflush(stderr);*/
    }
    return(&c_cmd);
}


ClmCommand *ClmFdefineCursor(cmd)
ClmCommand *cmd;
{
    char   *msg;
    Widget  widget;
    int     i, mode;
    Cursor  cursorid;

    if(cmd->command != ClmDefineCursor)
	GenError("DefineCursor illegal command record");

    cursorid = IntArg0(cmd);


    for(i=1; i<cmd->num_arg; i++) {
        if( msg = LookupWidget(IntArgn(cmd,i), &widget, NULL, NULL) )
            GenWarn(msg);
	XDefineCursor(global.display, XtWindow(widget), 
                      cursorid == -1 ? None : cursorid);
        XFlush(global.display);
    }
    return(NULL);
}
