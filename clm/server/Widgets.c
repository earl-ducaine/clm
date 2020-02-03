static char sccsid[] = "@(#)Widgets.c	1.7 1/30/92";

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

#include <X11/IntrinsicP.h>
#include <X11/StringDefs.h>
#include <X11/CompositeP.h>

#include <Xm/Xm.h>

#include "interface.h"
#include "functions.h"

#define MAX_EXTERNAL_WIDGETS 1000000
#define MAX_INTERNAL_WIDGETS 2000000

#define HASH_TABLE_SIZE 2000

static WidgetNode  WidgetTable[HASH_TABLE_SIZE];

static WidgetNode DestroyList = {0, NULL, 0, 0, NULL, NULL, NULL, NULL};

static int NumDestroy, NumWidgets;

WidgetID GenWidgetID()
{
    static WidgetID id = MAX_EXTERNAL_WIDGETS;

    return(id++);
}

int sign(expr)
int expr;
{
    if(expr > 0) return(1);
    if(expr < 0) return(-1);
    return(0);
}

void InitializeWidgetTable()
{
    register int i;

    for(i=0; i < HASH_TABLE_SIZE; i++) {
	WidgetTable[i].w_next   = WidgetTable+i;
	WidgetTable[i].w_pre    = WidgetTable+i;
	WidgetTable[i].l_next   = WidgetTable+i;
	WidgetTable[i].l_pre    = WidgetTable+i;
	WidgetTable[i].widget = (Widget)NULL;
    }
}

char *InsertWidget(id, widget, class_id, parent_class_id)
WidgetID     id;
Widget       widget;
int          parent_class_id, class_id;
{
    int         l_slot, w_slot;
    WidgetNode *new;
    Widget      test_widget;

    if (LookupWidget (id, &test_widget, NULL, NULL) == NULL) {
	fprintf (stderr,
		 "Warning: ID %d inserted twice in widget table.\n", id);
	fflush (stderr);
    }
    l_slot = id%HASH_TABLE_SIZE;
    w_slot = (((unsigned int)widget) >> 2) % HASH_TABLE_SIZE;
    new = (WidgetNode *)ClmMalloc(sizeof(WidgetNode));
    new->id              = id;
    new->widget          = widget;
    new->class_id        = class_id;
    new->parent_class_id = parent_class_id;
    new->translations    = NULL;
    new->exposures       = NULL;
    new->motions         = NULL;
    new->l_pre           = WidgetTable+l_slot;
    new->l_next          = WidgetTable[l_slot].l_next;
    new->l_next->l_pre   = new;
    new->l_pre->l_next   = new;
    new->w_pre           = WidgetTable+w_slot;
    new->w_next          = WidgetTable[w_slot].w_next;
    new->w_next->w_pre   = new;
    new->w_pre->w_next   = new;
    NumWidgets++;
    return(NULL);
}

char *LookupWidget(id, widget_ptr, class_id, node_ptr)
register WidgetID     id;
         Widget      *widget_ptr;
         int         *class_id;
	 WidgetNode **node_ptr;
{
    register int         l_slot = id%HASH_TABLE_SIZE;
    register WidgetNode *node   = WidgetTable[l_slot].l_next;
    static   char        err_msg[50];

    if (id < 0) {
	sprintf(err_msg, "Illegal widget ID %d\n", id);
	return(err_msg);
    }

    while( node->id != id && node != WidgetTable+l_slot )
	node = node->l_next;

    if( node == WidgetTable+l_slot ) {
	sprintf(err_msg, "WidgetID %d not found", id);
	return(err_msg);
    }

    *widget_ptr = node->widget;
    if( class_id != NULL )
	*class_id = node->class_id;
    if( node_ptr != NULL )
	*node_ptr = node;
    return(NULL);
}

char *LookupClassOfParentOfWidget(id, parent_class_id)
WidgetID  id;
int      *parent_class_id;
{
    char       *msg;
    Widget     *w;
    WidgetNode *node;

    if( (msg = LookupWidget(id, &w, NULL, &node)) != NULL )
	return(msg);
    *parent_class_id = node->parent_class_id;
    return(NULL);
}

WidgetID FindWidget(Widget widget) {
  int w_slot = (((unsigned int)widget)>>2)%HASH_TABLE_SIZE;
  register WidgetNode *node   = WidgetTable[w_slot].w_next;

  while( node->widget != widget && node != WidgetTable+w_slot )
    node = node->w_next;

  if( node == WidgetTable+w_slot )
    return(-1);
  return(node->id);
}

static WidgetNode *FindNode(Widget widget) {
  int w_slot = (((unsigned int)widget)>>2)%HASH_TABLE_SIZE;
  register WidgetNode *node   = WidgetTable[w_slot].w_next;
  while( node->widget != widget && node != WidgetTable+w_slot )
    node = node->w_next;
  if( node == WidgetTable+w_slot )
    return(NULL);
  return(node);
}

void DestroyWidget(widget,client_data,call_data)
Widget widget;
caddr_t client_data, call_data;
{
    WidgetID id = (WidgetID)client_data;
    WidgetNode *node;

    if( (node = FindNode(widget)) == NULL)
	abort();

    node->l_pre->l_next = node->l_next;
    node->l_next->l_pre = node->l_pre;
    node->w_pre->w_next = node->w_next;
    node->w_next->w_pre = node->w_pre;
    node->w_next = DestroyList.w_next;
    DestroyList.w_next = node;
    NumDestroy++;
}

static ClmCommand DCBC = {ClmEvent, -1, 0, NULL};

void NotifyDestroyed()
{
    register ClmArg     *args;
    register int         i;
    register WidgetNode *wn, *wnd;

    args = DCBC.args = (ClmArg *)ClmMalloc((NumDestroy+1)*sizeof(ClmArg));
    DCBC.num_arg = NumDestroy+1;

    args[0].arg_type       = ClmArgInteger;
    args[0].v.int_value    = ClmEventDestroy;

    for( i=1, wn = DestroyList.w_next; wn != NULL; i++, wn = wn->w_next ) {
	args[i].arg_type    = ClmArgInteger;
	args[i].v.int_value = wn->id;
    }

    if(SendCommand(global.socket, &DCBC) == -1)
        abort();
    XtFree(args);

    for(wn = DestroyList.w_next; wn != NULL; wn = wnd) {
	wnd = wn->w_next;
	XtFree(wn);
    }
    DestroyList.w_next = NULL;
    NumWidgets -= NumDestroy;
    NumDestroy = 0;
}

void RegisterDestroyCallback(widget,id)
Widget   widget;
WidgetID id;
{
    XtAddCallback(widget, XtNdestroyCallback, DestroyWidget, (caddr_t)id);
}
