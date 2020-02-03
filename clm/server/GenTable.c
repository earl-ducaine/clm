static char sccsid[] = "@(#)GenTable.c	1.9 9/8/93";

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
#include <ctype.h>

#include <X11/IntrinsicP.h>
#include <X11/StringDefs.h>

#include <X11/ConstrainP.h>
#include <X11/Core.h>
#include <X11/Shell.h>

#include <X11/Object.h>

#include <Xm/ArrowB.h>
#include <Xm/ArrowBG.h>
#include <Xm/BulletinB.h>
#include <Xm/CascadeB.h>
#include <Xm/CascadeBG.h>
#include <Xm/Command.h>
#include <Xm/DialogS.h>
#include <Xm/DrawingA.h>
#include <Xm/DrawnB.h>
#include <Xm/FileSB.h>
#include <Xm/Form.h>
#include <Xm/Frame.h>
#include <Xm/Label.h>
#include <Xm/LabelG.h>
#include <Xm/List.h>
#include <Xm/MainW.h>
#include <Xm/MenuShell.h>
#include <Xm/MessageB.h>
#include <Xm/PanedW.h>
#include <Xm/PushB.h>
#include <Xm/PushBG.h>
#include <Xm/RowColumn.h>
#include <Xm/Scale.h>
#include <Xm/ScrollBar.h>
#include <Xm/ScrolledW.h>
#include <Xm/SelectioB.h>
#include <Xm/SeparatoG.h>
#include <Xm/Separator.h>
#include <Xm/Text.h>
#include <stdlib.h>

#if XmREVISION != 0
#include <Xm/TextF.h>
#endif

#include <Xm/ToggleB.h>
#include <Xm/ToggleBG.h>

#ifdef GRAPHWIDGET
#include <Xbab/Graph.h>
#endif

#include "interface.h"
#include "functions.h"
#include "lisp.h"

static int num_classes;

ClassEntry ClassTable[MAXCLASSES];
char       *PointerNames[MAXCLASSES];

static int BootClass(char* name, WidgetClass class,
		     char* pointer_name) {
  // test table overflow
  if(num_classes == MAXCLASSES) {
    fprintf(stderr,"Class table overflow\n");
    exit(1);
  }
#if XmREVISION != 0
  XtInitializeWidgetClass(class);
#endif
  // insert class at end of table
  ClassTable[num_classes].class = class;
  ClassTable[num_classes].total_resources = 0;
  ClassTable[num_classes].name = name;
  PointerNames[num_classes] = pointer_name;
  return num_classes++;
}

// Convert resource name to external name keyboardFocusType -->
// KEYBOARD-FOCUS-TYPE

char *ExternalName(char* name) {
  char* s = name;
  char* external_name;
  int length=0;
  while(*s) {
    if(isupper(*s) && s != name )
      length ++;
    length++;
    s++;
  }
  if((external_name = (char *)malloc(length+1)) == NULL) {
    perror("malloc");
    exit(1);
  }
  s = external_name;
  while(*name) {
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

/* Get index of resource type (String) in type table */

int RepIndex(resource_type)
char *resource_type;
{
    int i;

    for(i=0; TypeTable[i].resource_type != NULL; i++ )
	if( ! strcmp(TypeTable[i].resource_type, resource_type) )
	    return(i);
    return(-1);
}

void AddResource(i, j, name, type)
int i, j;
char *name, *type;
{
    ClassTable[i].all_resources[j].resource_name = name;
    ClassTable[i].all_resources[j].external_name = ExternalName(name);
    if( (ClassTable[i].all_resources[j].representation =
	 RepIndex(type) ) == (-1) )
	fprintf(stderr, "Warning: No resource converter for class %s\n", type);
}

void AddExtraResource(i, name, type)
int i;
char *name, *type;
{
    ClassTable[i].total_resources++;
    AddResource(i, ClassTable[i].total_resources - 1, name, type);
}

void AddConstraint(i, j, name, type)
int i, j;
char *name, *type;
{
    ClassTable[i].all_constraints[j].resource_name = name;
    ClassTable[i].all_constraints[j].external_name = ExternalName(name);
    if( (ClassTable[i].all_constraints[j].representation =
	 RepIndex(type) ) == (-1) )
	fprintf(stderr, "Warning: No resource converter for class %s\n", type);
}


#if XmREVISION == 0
char *InitializeClassTable()
{
    XtResourceList r;
    ClmResourceDesc res;
    int i,j, k, n, total_resources, dummy=0, size;
    Display *display;
    Arg arg[10];
    WidgetClass class;
    XtAppContext app_context;

    XtToolkitInitialize();
    /* app_context = XtCreateApplicationContext(); */
    if( (display = XtOpenDisplay(NULL /*app_context*/, NULL,
				 "gentable", "GenTable",
				 NULL, 0, &dummy, NULL)) == NULL ) {
	fprintf(stderr,"FATAL: XtOpenDisplay failed"); fflush(stderr);
	abort();
    }

    /* MUST be sorted alphabeticaly by first argument */

    BootClass("APPLICATION-SHELL", applicationShellWidgetClass,
	      "applicationShellWidgetClass");
    BootClass("ARROW-BUTTON", xmArrowButtonWidgetClass,
	      "xmArrowButtonWidgetClass");
    BootClass("ARROW-BUTTON-GADGET", xmArrowButtonGadgetClass,
	      "xmArrowButtonGadgetClass");
    BootClass("BULLETIN-BOARD", xmBulletinBoardWidgetClass,
	      "xmBulletinBoardWidgetClass");
    BootClass("CASCADE-BUTTON", xmCascadeButtonWidgetClass,
	      "xmCascadeButtonWidgetClass");
    BootClass("CASCADE-BUTTON-GADGET", xmCascadeButtonGadgetClass,
	      "xmCascadeButtonGadgetClass");
    BootClass("COMMAND", xmCommandWidgetClass, "xmCommandWidgetClass");
    BootClass("DIALOG-SHELL", xmDialogShellWidgetClass,
	      "xmDialogShellWidgetClass");
    BootClass("DRAWING-AREA", xmDrawingAreaWidgetClass,
	      "xmDrawingAreaWidgetClass");
    BootClass("DRAWN-BUTTON", xmDrawnButtonWidgetClass,
	      "xmDrawnButtonWidgetClass");
    BootClass("FILE-SELECTION-BOX", xmFileSelectionBoxWidgetClass,
	      "xmFileSelectionBoxWidgetClass");
    BootClass("FORM", xmFormWidgetClass, "xmFormWidgetClass");
    BootClass("FRAME", xmFrameWidgetClass, "xmFrameWidgetClass");
#ifdef GRAPHWIDGET
    BootClass("GRAPH", xmGraphWidgetClass, "xmGraphWidgetClass");
#endif GRAPHWIDGET
    BootClass("LABEL", xmLabelWidgetClass, "xmLabelWidgetClass");
    BootClass("LABEL-GADGET", xmLabelGadgetClass, "xmLabelGadgetClass");
    BootClass("LIST", xmListWidgetClass, "xmListWidgetClass");
    BootClass("MAIN-WINDOW", xmMainWindowWidgetClass,
	      "xmMainWindowWidgetClass");
    BootClass("MENU-SHELL", xmMenuShellWidgetClass,
	      "xmMenuShellWidgetClass");
    BootClass("MESSAGE-BOX", xmMessageBoxWidgetClass,
	      "xmMessageBoxWidgetClass");
    BootClass("OVERRIDE-SHELL", overrideShellWidgetClass,
	      "overrideShellWidgetClass");
    BootClass("PANED-WINDOW", xmPanedWindowWidgetClass,
	      "xmPanedWindowWidgetClass");
    BootClass("PUSH-BUTTON", xmPushButtonWidgetClass,
	      "xmPushButtonWidgetClass");
    BootClass("PUSH-BUTTON-GADGET", xmPushButtonGadgetClass,
	      "xmPushButtonGadgetClass");
    BootClass("ROW-COLUMN", xmRowColumnWidgetClass, "xmRowColumnWidgetClass");
    BootClass("SCALE", xmScaleWidgetClass, "xmScaleWidgetClass");
    BootClass("SCROLL-BAR", xmScrollBarWidgetClass, "xmScrollBarWidgetClass");
    BootClass("SCROLLED-WINDOW", xmScrolledWindowWidgetClass,
	      "xmScrolledWindowWidgetClass");
    BootClass("SELECTION-BOX", xmSelectionBoxWidgetClass,
	      "xmSelectionBoxWidgetClass");
    BootClass("SEPARATOR", xmSeparatorWidgetClass,
	      "xmSeparatorWidgetClass");
    BootClass("SEPARATOR-GADGET", xmSeparatorGadgetClass,
	      "xmSeparatorGadgetClass");
    BootClass("TEXT", xmTextWidgetClass, "xmTextWidgetClass");
/*    BootClass("TEXT-FIELD", xmTextFieldWidgetClass, "xmTextFieldWidgetClass"); */
    BootClass("TOGGLE-BUTTON", xmToggleButtonWidgetClass,
	      "xmToggleButtonWidgetClass");
    BootClass("TOGGLE-BUTTON-GADGET", xmToggleButtonGadgetClass,
	      "xmToggleButtonGadgetClass");
    BootClass("TOPLEVEL-SHELL", topLevelShellWidgetClass,
	      "topLevelShellWidgetClass");
    BootClass("TRANSIENT-SHELL", transientShellWidgetClass,
	      "transientShellWidgetClass");

    XSynchronize(display, True);
    XSetErrorHandler(abort);

    /* Beginning of constain code */
    /* Do this here because constraints get messed up - so it seems */

    for( i=0; i<num_classes; i++ ) {
	ClassTable[i].constraint_list = NULL;
	ClassTable[i].total_constraints = 0;

	/* Check if class is subclass of Constraint */
	for( class = ClassTable[i].class;
	     class && class != constraintWidgetClass;
	     class = class->core_class.superclass );

	if( class == constraintWidgetClass ) {
	    /* Should be XtGetConstraints(...) */

	    ClassTable[i].total_constraints = ((ConstraintWidgetClass)
		ClassTable[i].class)->constraint_class.num_resources;
	    size =  ClassTable[i].total_constraints * sizeof(XtResource);
	    ClassTable[i].constraint_list =
		(XtResourceList) XtMalloc((unsigned) size);
	    bcopy((char *)(((ConstraintWidgetClass)
		  ClassTable[i].class)->constraint_class.resources),
		  (char *)(ClassTable[i].constraint_list), size);
	}
    }

    /* End of constraint code */

    /* Add this to cause the class to get initialized */

{
    Widget app_shell, row_column,dialog_shell,b_board;

    app_shell = XtAppCreateShell("D","D",applicationShellWidgetClass,display,NULL,0);

    XtCreatePopupShell("D",overrideShellWidgetClass,app_shell,NULL,0);
    XtCreatePopupShell("D",transientShellWidgetClass,app_shell,NULL,0);
    XtCreatePopupShell("D",topLevelShellWidgetClass,app_shell,NULL,0);
    dialog_shell = XtCreatePopupShell("D",xmDialogShellWidgetClass,app_shell,NULL,0);

    n=0; XtSetArg(arg[n],XmNrowColumnType,XmMENU_BAR);
    row_column = XtCreateWidget("D", xmRowColumnWidgetClass, app_shell, arg, n);

    n=0; XtSetArg(arg[n], XmNwidth, 5);
    n++; XtSetArg(arg[n], XmNheight, 5);
    n++; XtSetArg(arg[n], XmNoverrideRedirect, True);
    n++; XtSetArg(arg[n], XmNallowShellResize, True);
    XtCreatePopupShell("D",xmMenuShellWidgetClass,row_column,arg,n);

    XtCreateWidget("cascade",xmCascadeButtonWidgetClass,row_column,NULL,0);
    XtCreateWidget("drawing",xmDrawingAreaWidgetClass,row_column,NULL,0);
    XtCreateWidget("label", xmLabelWidgetClass, row_column, NULL, 0);
    XtCreateWidget("toggle",xmToggleButtonWidgetClass,row_column, NULL, 0);
    XtCreateWidget("push", xmPushButtonWidgetClass, row_column, NULL, 0);
    XtCreateWidget("arrow", xmArrowButtonWidgetClass, row_column, NULL, 0);
    XtCreateWidget("draw", xmDrawnButtonWidgetClass, row_column, NULL, 0);
    XtCreateWidget("D", xmScrollBarWidgetClass, row_column, NULL, 0);
    XtCreateWidget("D", xmSeparatorWidgetClass, row_column, NULL, 0);
    XtCreateWidget("text", xmTextWidgetClass, row_column, NULL, 0);
    XtCreateWidget("D", xmFrameWidgetClass, row_column, NULL, 0);
    XtCreateWidget("D", xmListWidgetClass, row_column, NULL, 0);
    XtCreateWidget("D", xmMainWindowWidgetClass, row_column, NULL, 0);
    XtCreateWidget("D", xmRowColumnWidgetClass, row_column, NULL, 0);
    XtCreateWidget("D", xmScaleWidgetClass, row_column, NULL, 0);
    XtCreateWidget("D", xmScrolledWindowWidgetClass, row_column, NULL, 0);
    XtCreateWidget("D", xmPanedWindowWidgetClass, row_column, NULL, 0);

    b_board = XtCreateWidget("D", xmBulletinBoardWidgetClass,dialog_shell, NULL, 0);
    XtCreateWidget("D", xmCommandWidgetClass, b_board, NULL, 0);
    XtCreateWidget("D", xmFileSelectionBoxWidgetClass,b_board,NULL, 0);
    XtCreateWidget("D", xmFormWidgetClass, b_board, NULL, 0);
    XtCreateWidget("D", xmMessageBoxWidgetClass, b_board, NULL, 0);
    XtCreateWidget("D", xmSelectionBoxWidgetClass, b_board, NULL, 0);
    }

    for( i=0; i<num_classes; i++ ) {
	XtGetResourceList(ClassTable[i].class,
			  &(ClassTable[i].resource_list),
			  &(ClassTable[i].total_resources) );
/*	XtGetConstraintResourceList(ClassTable[i].class,
			  &(ClassTable[i].constraint_list),
			  &(ClassTable[i].total_constraints) ) */;
    }



    for( i=0; i<num_classes; i++ ) {

	/* Get some mem ... */
	/* Get extra memory for additional resources */
	if( (ClassTable[i].all_resources =
		     (ClmResourceDesc *) XtMalloc(sizeof(ClmResourceDesc)*
		     (ClassTable[i].total_resources+100))) == NULL ||
	    (ClassTable[i].all_constraints =
		     (ClmResourceDesc *) XtMalloc(sizeof(ClmResourceDesc)*
		     (ClassTable[i].total_constraints+100))) == NULL )
	    perror("malloc");

	/* Collect complete resource list from superclasses */

	for( j = 0; j < ClassTable[i].total_resources; j++ ) {
	    AddResource(i, j, ClassTable[i].resource_list[j].resource_name,
			ClassTable[i].resource_list[j].resource_type);
	}

	/* Add VendorShell's extended resources */
	/* This could be done with XmGetSecondaryResourceData(), but this
	   function is buggy (returns an invalid pointer ) */
	if(ClassTable[i].class == applicationShellWidgetClass ||
	   ClassTable[i].class == topLevelShellWidgetClass ||
	   ClassTable[i].class == transientShellWidgetClass ||
	   ClassTable[i].class == xmDialogShellWidgetClass ) {
	    AddExtraResource(i, XmNdeleteResponse, XmRDeleteResponse);
	    AddExtraResource(i, XmNkeyboardFocusPolicy, XmRKeyboardFocusPolicy);
	    AddExtraResource(i, XmNshellUnitType, XmRShellUnitType);
	}
	else if( ClassTable[i].class == xmTextWidgetClass ) {
	    /* Add text widget's special resources */
	    AddExtraResource(i, XmNpendingDelete, XmRBoolean);
	    AddExtraResource(i, XmNselectThreshold, XmRInt);
	    AddExtraResource(i, XmNblinkRate, XmRInt);
	    AddExtraResource(i, XmNcolumns, XmRShort);
	    AddExtraResource(i, XmNcursorPositionVisible, XmRBoolean);
	    AddExtraResource(i, XmNfontList, XmRFontList);
	    AddExtraResource(i, XmNresizeHeight, XmRBoolean);
	    AddExtraResource(i, XmNresizeWidth, XmRBoolean);
	    AddExtraResource(i, XmNrows, XmRShort);
	    AddExtraResource(i, XmNwordWrap, XmRBoolean);
	    AddExtraResource(i, XmNscrollHorizontal, XmRBoolean);
	    AddExtraResource(i, XmNscrollVertical, XmRBoolean);
	    AddExtraResource(i, XmNscrollLeftSide, XmRBoolean);
	    AddExtraResource(i, XmNscrollTopSide, XmRBoolean);
	}


	/* Build resource descriptors for constraints */

	for( j=0; j<ClassTable[i].total_constraints; j++ ) {
	    AddConstraint(i, j, ClassTable[i].constraint_list[j].resource_name,
			  ClassTable[i].constraint_list[j].resource_type);
	}

	/* Sort resources */

	if( ClassTable[i].total_resources > 0 )
	    SortResources(ClassTable[i].all_resources,
			  0, ClassTable[i].total_resources-1);
	if( ClassTable[i].total_constraints > 0 )
	    SortResources(ClassTable[i].all_constraints,
			  0, ClassTable[i].total_constraints-1);
    }
}
#else
char *InitializeClassTable()
{
    XtResourceList r;
    ClmResourceDesc res;
    int i,j, k, n, total_resources, dummy=0, size;
    Display *display;
    Arg arg[10];
    XtAppContext app_context;

    XtToolkitInitialize();
    app_context = XtCreateApplicationContext();
    if( (display = XtOpenDisplay(app_context, NULL, "gentable", "GenTable",
				 NULL, 0, &dummy, NULL)) == NULL ) {
	fprintf(stderr,"FATAL: XtOpenDisplay failed"); fflush(stderr);
	abort();
    }

    /* create a dummy application shell because Motif 1.2 performs
       some initializations there */

    XtAppCreateShell (NULL, "GenTable", applicationShellWidgetClass,
                      display, arg, 0);

    /* MUST be sorted alphabeticaly by first argument */

    BootClass("APPLICATION-SHELL", applicationShellWidgetClass,
	      "applicationShellWidgetClass");
    BootClass("ARROW-BUTTON", xmArrowButtonWidgetClass,
	      "xmArrowButtonWidgetClass");
    BootClass("ARROW-BUTTON-GADGET", xmArrowButtonGadgetClass,
	      "xmArrowButtonGadgetClass");
    BootClass("BULLETIN-BOARD", xmBulletinBoardWidgetClass,
	      "xmBulletinBoardWidgetClass");
    BootClass("CASCADE-BUTTON", xmCascadeButtonWidgetClass,
	      "xmCascadeButtonWidgetClass");
    BootClass("CASCADE-BUTTON-GADGET", xmCascadeButtonGadgetClass,
	      "xmCascadeButtonGadgetClass");
    BootClass("COMMAND", xmCommandWidgetClass, "xmCommandWidgetClass");
    BootClass("DIALOG-SHELL", xmDialogShellWidgetClass,
	      "xmDialogShellWidgetClass");
    BootClass("DRAWING-AREA", xmDrawingAreaWidgetClass,
	      "xmDrawingAreaWidgetClass");
    BootClass("DRAWN-BUTTON", xmDrawnButtonWidgetClass,
	      "xmDrawnButtonWidgetClass");
    BootClass("FILE-SELECTION-BOX", xmFileSelectionBoxWidgetClass,
	      "xmFileSelectionBoxWidgetClass");
    BootClass("FORM", xmFormWidgetClass, "xmFormWidgetClass");
    BootClass("FRAME", xmFrameWidgetClass, "xmFrameWidgetClass");
#ifdef GRAPHWIDGET
    BootClass("GRAPH", xmGraphWidgetClass, "xmGraphWidgetClass");
#endif
    BootClass("LABEL", xmLabelWidgetClass, "xmLabelWidgetClass");
    BootClass("LABEL-GADGET", xmLabelGadgetClass, "xmLabelGadgetClass");
    BootClass("LIST", xmListWidgetClass, "xmListWidgetClass");
    BootClass("MAIN-WINDOW", xmMainWindowWidgetClass,
	      "xmMainWindowWidgetClass");
    BootClass("MENU-SHELL", xmMenuShellWidgetClass,
	      "xmMenuShellWidgetClass");
    BootClass("MESSAGE-BOX", xmMessageBoxWidgetClass,
	      "xmMessageBoxWidgetClass");
    BootClass("OVERRIDE-SHELL", overrideShellWidgetClass,
	      "overrideShellWidgetClass");
    BootClass("PANED-WINDOW", xmPanedWindowWidgetClass,
	      "xmPanedWindowWidgetClass");
    BootClass("PUSH-BUTTON", xmPushButtonWidgetClass,
	      "xmPushButtonWidgetClass");
    BootClass("PUSH-BUTTON-GADGET", xmPushButtonGadgetClass,
	      "xmPushButtonGadgetClass");
    BootClass("ROW-COLUMN", xmRowColumnWidgetClass, "xmRowColumnWidgetClass");
    BootClass("SCALE", xmScaleWidgetClass, "xmScaleWidgetClass");
    BootClass("SCROLL-BAR", xmScrollBarWidgetClass, "xmScrollBarWidgetClass");
    BootClass("SCROLLED-WINDOW", xmScrolledWindowWidgetClass,
	      "xmScrolledWindowWidgetClass");
    BootClass("SELECTION-BOX", xmSelectionBoxWidgetClass,
	      "xmSelectionBoxWidgetClass");
    BootClass("SEPARATOR", xmSeparatorWidgetClass,
	      "xmSeparatorWidgetClass");
    BootClass("SEPARATOR-GADGET", xmSeparatorGadgetClass,
	      "xmSeparatorGadgetClass");
    BootClass("TEXT", xmTextWidgetClass, "xmTextWidgetClass");

    BootClass("TEXT-FIELD", xmTextFieldWidgetClass, "xmTextFieldWidgetClass");


    BootClass("TOGGLE-BUTTON", xmToggleButtonWidgetClass,
	      "xmToggleButtonWidgetClass");

    BootClass("TOGGLE-BUTTON-GADGET", xmToggleButtonGadgetClass,
	      "xmToggleButtonGadgetClass");
    BootClass("TOPLEVEL-SHELL", topLevelShellWidgetClass,
	      "topLevelShellWidgetClass");
    BootClass("TRANSIENT-SHELL", transientShellWidgetClass,
	      "transientShellWidgetClass");

    XSynchronize(display, True);
    XSetErrorHandler(abort);

    for( i=0; i<num_classes; i++ ) {
	XtGetResourceList(ClassTable[i].class,
			  &(ClassTable[i].resource_list),
			  &(ClassTable[i].total_resources) );
	XtGetConstraintResourceList(ClassTable[i].class,
			  &(ClassTable[i].constraint_list),
			  &(ClassTable[i].total_constraints) );
    }

    for( i=0; i<num_classes; i++ ) {

	/* Get some mem ... */
	/* Get extra memory for additional resources */
	if( (ClassTable[i].all_resources =
		     (ClmResourceDesc *) XtMalloc(sizeof(ClmResourceDesc)*
		     (ClassTable[i].total_resources+100))) == NULL ||
	    (ClassTable[i].all_constraints =
		     (ClmResourceDesc *) XtMalloc(sizeof(ClmResourceDesc)*
		     (ClassTable[i].total_constraints+100))) == NULL )
	    perror("malloc");

	/* Collect complete resource list from superclasses */

	for( j = 0; j < ClassTable[i].total_resources; j++ ) {
	    AddResource(i, j, ClassTable[i].resource_list[j].resource_name,
			ClassTable[i].resource_list[j].resource_type);
	}

	/* Add VendorShell's extended resources */
	/* This could be done with XmGetSecondaryResourceData(), but this
	   function is buggy (returns an invalid pointer ) */
	if(ClassTable[i].class == applicationShellWidgetClass ||
	   ClassTable[i].class == topLevelShellWidgetClass ||
	   ClassTable[i].class == transientShellWidgetClass ||
	   ClassTable[i].class == xmDialogShellWidgetClass ) {
	    AddExtraResource(i, XmNdeleteResponse, XmRDeleteResponse);
	    AddExtraResource(i, XmNdefaultFontList, XmRFontList);
	    AddExtraResource(i, XmNkeyboardFocusPolicy,XmRKeyboardFocusPolicy);
	    AddExtraResource(i, XmNmwmDecorations, XtRInt);
	    AddExtraResource(i, XmNmwmFunctions, XtRInt);
	    AddExtraResource(i, XmNmwmInputMode, XtRInt);
	    AddExtraResource(i, XmNmwmMenu, XtRInt);
	    AddExtraResource(i, XmNshellUnitType, XmRShellUnitType);
	    AddExtraResource(i, XmNuseAsyncGeometry, XmRBoolean);
	}
	else if( ClassTable[i].class == xmTextWidgetClass ) {
	    /* Add text widget's special resources */
	    AddExtraResource(i, XmNpendingDelete, XmRBoolean);
	    AddExtraResource(i, XmNselectThreshold, XmRInt);
	    AddExtraResource(i, XmNblinkRate, XmRInt);
	    AddExtraResource(i, XmNcolumns, XmRShort);
	    AddExtraResource(i, XmNcursorPositionVisible, XmRBoolean);
	    AddExtraResource(i, XmNfontList, XmRFontList);
	    AddExtraResource(i, XmNresizeHeight, XmRBoolean);
	    AddExtraResource(i, XmNresizeWidth, XmRBoolean);
	    AddExtraResource(i, XmNrows, XmRShort);
	    AddExtraResource(i, XmNwordWrap, XmRBoolean);
	    AddExtraResource(i, XmNscrollHorizontal, XmRBoolean);
	    AddExtraResource(i, XmNscrollVertical, XmRBoolean);
	    AddExtraResource(i, XmNscrollLeftSide, XmRBoolean);
	    AddExtraResource(i, XmNscrollTopSide, XmRBoolean);
	}


	/* Build resource descriptors for constraints */

	for( j=0; j<ClassTable[i].total_constraints; j++ ) {
	    AddConstraint(i, j, ClassTable[i].constraint_list[j].resource_name,
			  ClassTable[i].constraint_list[j].resource_type);
	}

	/* Sort resources */

	if( ClassTable[i].total_resources > 0 )
	    SortResources(ClassTable[i].all_resources,
			  0, ClassTable[i].total_resources-1);
	if( ClassTable[i].total_constraints > 0 )
	    SortResources(ClassTable[i].all_constraints,
			  0, ClassTable[i].total_constraints-1);
    }
}
#endif

SortResources(rp, left, right)
ClmResourceDesc *rp;
int              left, right;
{
             ClmResourceDesc  res;
    register int              i = left, j = right;
    register char            *ref = rp[(left+right)/2].external_name;

    do {
	while( strcmp(rp[i].external_name,ref) < 0 ) ++i;
	while( strcmp(ref,rp[j].external_name) < 0 ) --j;
	if( i <= j ) {
	    res.resource_name = rp[j].resource_name;
	    res.external_name = rp[j].external_name;
	    res.representation = rp[j].representation;
	    rp[j].resource_name = rp[i].resource_name;
	    rp[j].external_name = rp[i].external_name;
	    rp[j].representation = rp[i].representation;
	    rp[i].resource_name = res.resource_name;
	    rp[i].external_name = res.external_name;
	    rp[i].representation = res.representation;
	    ++i;
	    --j;
	}
    } while ( i <= j );

    if( left < j )
	SortResources(rp, left, j);
    if( i < right )
	SortResources(rp, i, right);
}

#define SKIP fputs("\n", module)

static void PrintClassTable()
{
    FILE *module, *lisp_module;
    int   i, j;

    if( (module = fopen(CLASS_TABLE_MODULE, "w")) == NULL ) {
	perror("fopen");
	exit(1);
    }

    fputs("#include <stdio.h>\n", module);
    fputs("#include <ctype.h>\n", module);
    SKIP;
    fputs("#include <X11/IntrinsicP.h>\n", module);
    fputs("#include <X11/StringDefs.h>\n", module);
    fputs("#include <X11/ConstrainP.h>\n", module);
    fputs("#include <X11/Core.h>\n", module);
    fputs("#include <X11/Shell.h>\n", module);
    fputs("#include <X11/Object.h>\n", module);
    fputs("#include <Xm/Label.h>\n", module);
    fputs("#include <Xm/LabelG.h>\n", module);
    fputs("#include <Xm/ArrowB.h>\n", module);
    fputs("#include <Xm/ArrowBG.h>\n", module);
    fputs("#include <Xm/DrawnB.h>\n", module);
    fputs("#include <Xm/PushB.h>\n", module);
    fputs("#include <Xm/PushBG.h>\n", module);
    fputs("#include <Xm/ToggleB.h>\n", module);
    fputs("#include <Xm/ToggleBG.h>\n", module);
    fputs("#include <Xm/CascadeB.h>\n", module);
    fputs("#include <Xm/CascadeBG.h>\n", module);
    fputs("#include <Xm/MenuShell.h>\n", module);
    fputs("#include <Xm/DrawingA.h>\n", module);
    fputs("#include <Xm/DialogS.h>\n", module);
    fputs("#include <Xm/BulletinB.h>\n", module);
    fputs("#include <Xm/Command.h>\n", module);
    fputs("#include <Xm/FileSB.h>\n", module);
    fputs("#include <Xm/Form.h>\n", module);
    fputs("#include <Xm/MessageB.h>\n", module);
    fputs("#include <Xm/SelectioB.h>\n", module);
    fputs("#include <Xm/ScrollBar.h>\n", module);
    fputs("#include <Xm/Separator.h>\n", module);
    fputs("#include <Xm/SeparatoG.h>\n", module);
    fputs("#include <Xm/Text.h>\n", module);
#if XmREVISION != 0
    fputs("#include <Xm/TextF.h>\n", module);
#endif
    fputs("#include <Xm/RowColumn.h>\n", module);
    fputs("#include <Xm/Scale.h>\n", module);
    fputs("#include <Xm/Frame.h>\n", module);
    fputs("#include <Xm/List.h>\n", module);
    fputs("#include <Xm/MainW.h>\n", module);
    fputs("#include <Xm/ScrolledW.h>\n", module);
    fputs("#include <Xm/PanedW.h>\n", module);
#ifdef GRAPHWIDGET
    fputs("#include <Xbab/Graph.h>\n", module);
#endif GRAPHWIDGET
    SKIP;
    fputs("#include \"interface.h\"\n", module);
    fputs("#include \"functions.h\"\n", module);
    SKIP;

    for( i=0; i<num_classes; i++ ) {
        fprintf(module, "static ClmResourceDesc %sResources[%d] = {\n",
		PointerNames[i], ClassTable[i].total_resources);
	for( j=0; j<ClassTable[i].total_resources; j++) {
	    fprintf(module, "    { \"%s\", \"%s\", %d },\n",
		    ClassTable[i].all_resources[j].resource_name,
		    ClassTable[i].all_resources[j].external_name,
		    ClassTable[i].all_resources[j].representation);
	}
	fputs("};\n", module);
	SKIP;
        if( ClassTable[i].total_constraints > 0 ) {
	    fprintf(module, "static ClmResourceDesc %sConstraints[%d] = {\n",
		    PointerNames[i], ClassTable[i].total_constraints);
	    for( j=0; j<ClassTable[i].total_constraints; j++) {
		fprintf(module, "    { \"%s\", \"%s\", %d },\n",
			ClassTable[i].all_constraints[j].resource_name,
			ClassTable[i].all_constraints[j].external_name,
			ClassTable[i].all_constraints[j].representation);
	    }
	    fputs("};\n", module);
	    SKIP;
	}
    }

    fprintf(module, "static int num_classes = %d;\n", num_classes);
    SKIP;
    fprintf(module, "ClassEntry ClassTable[MAXCLASSES] = {\n");

    for( i=0; i<num_classes; i++ ) {
	fprintf(module, "    { \"%s\",\n", ClassTable[i].name);
	fprintf(module, "      (WidgetClass)(&%s),\n", PointerNames[i]);
	fprintf(module, "      NULL, 0,\n");
        fprintf(module, "      NULL, %sResources, %d,\n",
			PointerNames[i], ClassTable[i].total_resources);

	fprintf(module, "      NULL, %s%s, %d},\n",
		ClassTable[i].total_constraints > 0 ? PointerNames[i] : "NULL",
		ClassTable[i].total_constraints > 0 ? "Constraints" : "",
		ClassTable[i].total_constraints);

    }
    fputs( "};\n", module);
    fclose(module);
}

main(argc,argv)
int argc;
char **argv;
{
    InitializeClassTable();
    PrintClassTable();
    exit(0); /* to make make happy */
}
