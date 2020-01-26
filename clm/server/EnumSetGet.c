static char sccsid[] = "@(#)EnumSetGet.c	1.7 9/8/93";

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
#include <X11/StringDefs.h>
#include <X11/Vendor.h>
#include <Xm/DialogSP.h>
#include <Xm/XmP.h>

#ifdef GRAPHWIDGET
#include <Xbab/Graph.h>
#endif GRAPHWIDGET

#include "interface.h"
#include "functions.h"

typedef struct _StringEnum {
    char          *name;
    unsigned char  value;
} StringEnum;

#define SEA(array) (array), (sizeof(array)/sizeof(StringEnum))

static caddr_t ConvertStringToEnum(name, msg_ptr, r_name, desc, n_desc)
char *name;
char **msg_ptr;
char *r_name;
StringEnum *desc;
int n_desc;
{
    int i;
    static char resource_error[300];
    char *str;
    
    for( i = 0; i < n_desc; i++ )
	if( ! strcmp(name, (desc + i)->name) )
	    return (caddr_t)((desc + i)->value);
    sprintf(resource_error,
	    "Illegal resource value for %s. Possible values are: ",
	    r_name);
    for( i = 0; i < n_desc; i++ )
	sprintf(resource_error + strlen(resource_error),
		":%s ", (desc + i)->name);
    *msg_ptr = resource_error;
    return (caddr_t)0;
}

static void ConvertEnumToString(resource, widget, type_ptr, value_ptr,
				msg_ptr, desc, n_desc)
char        *resource;
Widget       widget;
int         *type_ptr;
caddr_t     *value_ptr;
char       **msg_ptr;
StringEnum  *desc;
int          n_desc;
{
    int i;
    Arg arg[1];
    unsigned char value;

    XtSetArg(arg[0], resource, &value);
    XtGetValues(widget, arg, 1);
    for( i = 0; i < n_desc; i++ ) {
	if( value == (desc + i)->value ) {
	    *type_ptr  = ClmArgSymbol;
	    *value_ptr = (caddr_t)((desc + i)->name);
	    *msg_ptr   = NULL;
	    return;
	}
    }
    *msg_ptr = "Illegal resource value";
}

#define DEF_ENUM_CONVERTER(RESOURCE)                                    \
caddr_t _NAME2_(ClmPCvt,RESOURCE)(value, msg_ptr)                       \
caddr_t value;                                                          \
char **msg_ptr;                                                         \
{          							        \
    return ConvertStringToEnum(value, msg_ptr, _QUOTE_(RESOURCE),       \
                               SEA(_NAME2_(RESOURCE,Strings)));         \
}                                                                       \
void _NAME2_(ClmTCvt,RESOURCE)(name,widget,type_ptr,value_ptr,msg_ptr)  \
char     *name;                                                         \
char    **msg_ptr;                                                      \
{                                                                       \
    ConvertEnumToString(name, widget, type_ptr, value_ptr, msg_ptr,     \
			SEA(_NAME2_(RESOURCE,Strings)));                \
}

static StringEnum FileTypeMaskStrings[] =
#if XmREVISION == 0
    { { "DUMMY",          0 } };
#else
    { { "FILE-REGULAR",   XmFILE_REGULAR },
      { "FILE-DIRECTORY", XmFILE_DIRECTORY },
      { "FILE-ANY-TYPE",  XmFILE_ANY_TYPE },
      /* for compatibility */
      { "REGULAR",        XmFILE_REGULAR },
      { "DIRECTORY",      XmFILE_DIRECTORY },
      { "ANY-TYPE",       XmFILE_ANY_TYPE } };
#endif

DEF_ENUM_CONVERTER(FileTypeMask)

static StringEnum CommandWindowLocationStrings[] = 
#if XmREVISION == 0
    { { "DUMMY",          0 } };
#else
    { { "COMMAND-ABOVE-WORKSPACE", XmCOMMAND_ABOVE_WORKSPACE },
      { "COMMAND-BELOW-WORKSPACE", XmCOMMAND_BELOW_WORKSPACE },
      /* for compatibility */
      { "ABOVE",                   XmCOMMAND_ABOVE_WORKSPACE },
      { "BELOW",                   XmCOMMAND_BELOW_WORKSPACE } };
#endif

DEF_ENUM_CONVERTER(CommandWindowLocation)

static StringEnum MultiClickStrings[] = 
#if XmREVISION == 0
    { { "DUMMY",          0 } };
#else
    { { "MULTICLICK-KEEP",    XmMULTICLICK_KEEP },
      { "MULTICLICK-DISCARD", XmMULTICLICK_DISCARD },
      /* for compatibility */
      { "KEEP",               XmMULTICLICK_KEEP },
      { "DISCARD",            XmMULTICLICK_DISCARD } };
#endif

DEF_ENUM_CONVERTER(MultiClick)

static StringEnum InitialStateStrings[] = 
    { { "NORMAL-STATE", NormalState },
      { "ICONIC-STATE", IconicState } };

DEF_ENUM_CONVERTER(InitialState)

static StringEnum NavigationTypeStrings[] = 
#if XmREVISION == 0
    { { "DUMMY",          0 } };
#else
    { { "NONE",                XmNONE },
      { "STICKY-TAB-GROUP",    XmSTICKY_TAB_GROUP },
      { "TAB-GROUP",           XmTAB_GROUP },
      { "EXCLUSIVE-TAB-GROUP", XmEXCLUSIVE_TAB_GROUP } };
#endif

DEF_ENUM_CONVERTER(NavigationType)

static StringEnum SeparatorTypeStrings[] = 
    { { "SINGLE-LINE",        XmSINGLE_LINE },
      { "DOUBLE-LINE",        XmDOUBLE_LINE },
      { "DOUBLE-DASHED-LINE", XmDOUBLE_DASHED_LINE },
      { "SINGLE-DASHED-LINE", XmSINGLE_DASHED_LINE },
      { "NO-LINE",            XmNO_LINE },
      { "SHADOW-ETCHED-IN",   XmSHADOW_ETCHED_IN },
      { "SHADOW-ETCHED-OUT",  XmSHADOW_ETCHED_OUT } };

DEF_ENUM_CONVERTER(SeparatorType)

static StringEnum ProcessingDirectionStrings[] = 
    { { "MAX-ON-TOP",    XmMAX_ON_TOP },
      { "MAX-ON-LEFT",   XmMAX_ON_LEFT },
      { "MAX-ON-RIGHT",  XmMAX_ON_RIGHT },
      { "MAX-ON-BOTTOM", XmMAX_ON_BOTTOM } };

DEF_ENUM_CONVERTER(ProcessingDirection)

static StringEnum DefaultButtonTypeStrings[] = 
    { { "DIALOG-CANCEL-BUTTON", XmDIALOG_CANCEL_BUTTON },
      { "DIALOG-OK-BUTTON",     XmDIALOG_OK_BUTTON },
      { "DIALOG-HELP-BUTTON",   XmDIALOG_HELP_BUTTON },
      /* for compatibility */
      { "CANCEL",               XmDIALOG_CANCEL_BUTTON },
      { "OK",                   XmDIALOG_OK_BUTTON },
      { "HELP",                 XmDIALOG_HELP_BUTTON } };

DEF_ENUM_CONVERTER(DefaultButtonType)

static StringEnum ResizePolicyStrings[] = 
    { { "RESIZE-ANY",  XmRESIZE_ANY },
      { "RESIZE-GROW", XmRESIZE_GROW },
      { "RESIZE-NONE", XmRESIZE_NONE },
      /* for compatibility */
      { "ANY",         XmRESIZE_ANY },
      { "GROW",        XmRESIZE_GROW },
      { "NONE",        XmRESIZE_NONE } };

DEF_ENUM_CONVERTER(ResizePolicy)

static StringEnum ArrowDirectionStrings[] = 
    { { "ARROW-DOWN",  XmARROW_DOWN },
      { "ARROW-LEFT",  XmARROW_LEFT },
      { "ARROW-RIGHT", XmARROW_RIGHT },
      { "ARROW-UP",    XmARROW_UP } };

DEF_ENUM_CONVERTER(ArrowDirection)

static StringEnum ListSizePolicyStrings[] = 
    { { "CONSTANT",           XmCONSTANT },
      { "VARIABLE",           XmVARIABLE },
      { "RESIZE-IF-POSSIBLE", XmRESIZE_IF_POSSIBLE } };

DEF_ENUM_CONVERTER(ListSizePolicy)

static StringEnum UnitTypeStrings[] = 
    { { "100TH-MILLIMETERS", Xm100TH_MILLIMETERS },
      { "1000TH-INCHES",     Xm1000TH_INCHES },
      { "100TH-POINTS",      Xm100TH_POINTS },
      { "100TH-FONT-UNITS",  Xm100TH_FONT_UNITS },
      { "PIXELS",            XmPIXELS } };

DEF_ENUM_CONVERTER(UnitType)

static StringEnum IndicatorTypeStrings[] = 
    { { "ONE-OF-MANY", XmONE_OF_MANY },
      { "N-OF-MANY",   XmN_OF_MANY } };

DEF_ENUM_CONVERTER(IndicatorType)

static StringEnum SelectionPolicyStrings[] = 
    { { "SINGLE-SELECT",   XmSINGLE_SELECT },
      { "MULTIPLE-SELECT", XmMULTIPLE_SELECT },
      { "EXTENDED-SELECT", XmEXTENDED_SELECT },
      { "BROWSE-SELECT",   XmBROWSE_SELECT },
      /* for compatibility */
      { "SINGLE",          XmSINGLE_SELECT },
      { "MULTIPLE",        XmMULTIPLE_SELECT },
      { "EXTENDED",        XmEXTENDED_SELECT },
      { "BROWSE",          XmBROWSE_SELECT } };

DEF_ENUM_CONVERTER(SelectionPolicy)

static StringEnum StringDirectionStrings[] = 
    { { "STRING-DIRECTION-R-TO-L", XmSTRING_DIRECTION_R_TO_L },
      { "STRING-DIRECTION-L-TO-R", XmSTRING_DIRECTION_L_TO_R } };

DEF_ENUM_CONVERTER(StringDirection)

static StringEnum ShadowTypeStrings[] = 
    { { "SHADOW-IN",         XmSHADOW_IN },
      { "SHADOW-OUT",        XmSHADOW_OUT },
      { "SHADOW-ETCHED-IN",  XmSHADOW_ETCHED_IN },
      { "SHADOW-ETCHED-OUT", XmSHADOW_ETCHED_OUT },
      /* for compatibility */
      { "IN",                XmSHADOW_IN },
      { "OUT",               XmSHADOW_OUT },
      { "ETCHED-IN",         XmSHADOW_ETCHED_IN },
      { "ETCHED-OUT",        XmSHADOW_ETCHED_OUT } };

DEF_ENUM_CONVERTER(ShadowType)

static StringEnum DeleteResponseStrings[] = 
    { { "DESTROY",    XmDESTROY },
      { "UNMAP",      XmUNMAP },
      { "DO-NOTHING", XmDO_NOTHING } };

DEF_ENUM_CONVERTER(DeleteResponse)

static StringEnum EditModeStrings[] = 
    { { "MULTI-LINE-EDIT",  XmMULTI_LINE_EDIT },
      { "SINGLE-LINE-EDIT", XmSINGLE_LINE_EDIT } };

DEF_ENUM_CONVERTER(EditMode)

static StringEnum ScrollBarDisplayPolicyStrings[] = 
    { { "AS-NEEDED", XmAS_NEEDED },
      { "STATIC",    XmSTATIC } };

DEF_ENUM_CONVERTER(ScrollBarDisplayPolicy)

static StringEnum ScrollBarPlacementStrings[] = 
    { { "TOP-LEFT",     XmTOP_LEFT },
      { "TOP-RIGHT",    XmTOP_RIGHT },
      { "BOTTOM-LEFT",  XmBOTTOM_LEFT },
      { "BOTTOM-RIGHT", XmBOTTOM_RIGHT } };

DEF_ENUM_CONVERTER(ScrollBarPlacement)

static StringEnum ScrollingPolicyStrings[] = 
    { { "AUTOMATIC",           XmAUTOMATIC },
      { "APPLICATION-DEFINED", XmAPPLICATION_DEFINED } };

DEF_ENUM_CONVERTER(ScrollingPolicy)

static StringEnum VisualPolicyStrings[] = 
    { { "CONSTANT", XmCONSTANT },
      { "VARIABLE", XmVARIABLE } };

DEF_ENUM_CONVERTER(VisualPolicy)

static StringEnum AlignmentStrings[] = 
    { { "ALIGNMENT-CENTER",    XmALIGNMENT_CENTER },
      { "ALIGNMENT-END",       XmALIGNMENT_END },
      { "ALIGNMENT-BEGINNING", XmALIGNMENT_BEGINNING },
      /* for compatibility */ 
      { "CENTER",              XmALIGNMENT_CENTER },
      { "END",                 XmALIGNMENT_END },
      { "BEGINNING",           XmALIGNMENT_BEGINNING } };

DEF_ENUM_CONVERTER(Alignment)

static StringEnum LabelTypeStrings[] = 
    { { "STRING", XmSTRING },
      { "PIXMAP", XmPIXMAP } };

DEF_ENUM_CONVERTER(LabelType)

static StringEnum DialogStyleStrings[] = 
    { {"DIALOG-SYSTEM-MODAL",             XmDIALOG_SYSTEM_MODAL },
#if XmREVISION != 0
      {"DIALOG-APPLICATION-MODAL",        XmDIALOG_PRIMARY_APPLICATION_MODAL },
      {"DIALOG-PRIMARY-APPLICATION-MODAL",XmDIALOG_PRIMARY_APPLICATION_MODAL },
      {"DIALOG-FULL-APPLICATION-MODAL",   XmDIALOG_FULL_APPLICATION_MODAL },
#else
      {"DIALOG-APPLICATION-MODAL",        XmDIALOG_APPLICATION_MODAL },
      {"DIALOG-PRIMARY-APPLICATION-MODAL",XmDIALOG_APPLICATION_MODAL },
      {"DIALOG-FULL-APPLICATION-MODAL",   XmDIALOG_APPLICATION_MODAL },
#endif
      {"DIALOG-MODELESS",                 XmDIALOG_MODELESS },
      {"DIALOG-WORK-AREA",                XmDIALOG_WORK_AREA },
      /* for compatibility */
      {"SYSTEM-MODAL",                    XmDIALOG_SYSTEM_MODAL },
#if XmREVISION != 0
      {"APPLICATION-MODAL",               XmDIALOG_PRIMARY_APPLICATION_MODAL },
      {"PRIMARY-APPLICATION-MODAL",       XmDIALOG_PRIMARY_APPLICATION_MODAL },
      {"FULL-APPLICATION-MODAL",          XmDIALOG_FULL_APPLICATION_MODAL },
#else
      {"APPLICATION-MODAL",               XmDIALOG_APPLICATION_MODAL },
      {"PRIMARY-APPLICATION-MODAL",       XmDIALOG_APPLICATION_MODAL },
      {"FULL-APPLICATION-MODAL",          XmDIALOG_APPLICATION_MODAL },
#endif
      {"MODELESS",                        XmDIALOG_MODELESS },
      {"WORK-AREA",                       XmDIALOG_WORK_AREA } };

DEF_ENUM_CONVERTER(DialogStyle)

static StringEnum DialogTypeStrings[] = 
    { { "DIALOG-FILE-SELECTION", XmDIALOG_FILE_SELECTION },
      { "DIALOG-PROMPT",         XmDIALOG_PROMPT },
      { "DIALOG-SELECTION",      XmDIALOG_SELECTION },
      { "DIALOG-WORK-AREA",      XmDIALOG_WORK_AREA },
      { "DIALOG-ERROR",          XmDIALOG_ERROR },
      { "DIALOG-MESSAGE",        XmDIALOG_MESSAGE },
      { "DIALOG-QUESTION",       XmDIALOG_QUESTION },
      { "DIALOG-WARNING",        XmDIALOG_WARNING },
      { "DIALOG-INFORMATION",    XmDIALOG_INFORMATION },
      /* for compatibility */
      { "FILE-SELECTION",        XmDIALOG_FILE_SELECTION },
      { "PROMPT",                XmDIALOG_PROMPT },
      { "SELECTION",             XmDIALOG_SELECTION },
      { "WORK-AREA",             XmDIALOG_WORK_AREA },
      { "ERROR",                 XmDIALOG_ERROR },
      { "MESSAGE",               XmDIALOG_MESSAGE },
      { "QUESTION",              XmDIALOG_QUESTION },
      { "WARNING",               XmDIALOG_WARNING },
      { "INFORMATION",           XmDIALOG_INFORMATION } };

DEF_ENUM_CONVERTER(DialogType)

static StringEnum PackingStrings[] = 
    { { "PACK-TIGHT",  XmPACK_TIGHT },
      { "PACK-NONE",   XmPACK_NONE  },
      { "PACK-COLUMN", XmPACK_COLUMN } };

DEF_ENUM_CONVERTER(Packing)

static StringEnum AttachmentStrings[] = 
    { { "ATTACH-FORM",            XmATTACH_FORM },
      { "ATTACH-POSITION",        XmATTACH_POSITION },
      { "ATTACH-WIDGET",          XmATTACH_WIDGET },
      { "ATTACH-SELF",            XmATTACH_SELF },
      { "ATTACH-NONE",            XmATTACH_NONE },
      { "ATTACH-OPPOSITE-FORM",   XmATTACH_OPPOSITE_FORM },
      { "ATTACH-OPPOSITE-WIDGET", XmATTACH_OPPOSITE_WIDGET },
      { "FORM",            XmATTACH_FORM },
      { "POSITION",        XmATTACH_POSITION },
      { "WIDGET",          XmATTACH_WIDGET },
      { "SELF",            XmATTACH_SELF },
      { "NONE",            XmATTACH_NONE },
      { "OPPOSITE-FORM",   XmATTACH_OPPOSITE_FORM },
      { "OPPOSITE-WIDGET", XmATTACH_OPPOSITE_WIDGET } };

DEF_ENUM_CONVERTER(Attachment)

static StringEnum KeyboardFocusPolicyStrings[] = 
    { { "POINTER",  XmPOINTER },
      { "EXPLICIT", XmEXPLICIT } };

DEF_ENUM_CONVERTER(KeyboardFocusPolicy)

static StringEnum RowColumnTypeStrings[] = 
    { { "WORK-AREA",     XmWORK_AREA },
      { "MENU-BAR",      XmMENU_BAR },
      { "MENU-PULLDOWN", XmMENU_PULLDOWN },
      { "MENU-POPUP",    XmMENU_POPUP },
      { "MENU-OPTION",   XmMENU_OPTION } };

DEF_ENUM_CONVERTER(RowColumnType)

static StringEnum OrientationStrings[] = 
    { { "HORIZONTAL", XmHORIZONTAL },
      { "VERTICAL",   XmVERTICAL } };

DEF_ENUM_CONVERTER(Orientation)

#ifdef GRAPHWIDGET
static StringEnum AlgorithmStrings[] = 
    { { "NO-LAYOUT",  XmNO_LAYOUT },
      { "ISI-LAYOUT", XmISI_LAYOUT } };

DEF_ENUM_CONVERTER(Algorithm)

static StringEnum NodeTypeStrings[] = 
    { { "NORMAL-NODE",    XmNORMAL_NODE },
      { "INVISIBLE-NODE", XmINVISIBLE_NODE },
      { "VISIBLE-ROOT",   XmVISIBLE_ROOT },
      { "INVISIBLE-ROOT", XmINVISIBLE_ROOT } };

DEF_ENUM_CONVERTER(NodeType)

static StringEnum EdgeModeStrings[] = 
    { { "DIRECT",     XmDIRECT },
      { "ORTHOGONAL", XmORTHOGONAL } };

DEF_ENUM_CONVERTER(EdgeMode)

#endif GRAPHWIDGET

#if XmREVISION >= 2

static StringEnum ChildHorizontalAlignmentStrings[] = 
    { { "ALIGNMENT-BEGINNING", XmALIGNMENT_BEGINNING },
      { "ALIGNMENT-CENTER",    XmALIGNMENT_CENTER },
      { "ALIGNMENT-END",       XmALIGNMENT_END } };

DEF_ENUM_CONVERTER(ChildHorizontalAlignment)

static StringEnum ChildPlacementStrings[] = 
    { { "PLACE-TOP",             XmPLACE_TOP },
      { "PLACE-ABOVE-SELECTION", XmPLACE_ABOVE_SELECTION },
      { "PLACE-BELOW-SELECTION", XmPLACE_BELOW_SELECTION } };

DEF_ENUM_CONVERTER(ChildPlacement)

static StringEnum ChildTypeStrings[] = 
    { { "FRAME-GENERIC-CHILD",  XmFRAME_GENERIC_CHILD },
      { "FRAME-WORKAREA-CHILD", XmFRAME_WORKAREA_CHILD },
      { "FRAME-TITLE-CHILD",    XmFRAME_TITLE_CHILD } };

DEF_ENUM_CONVERTER(ChildType)

static StringEnum ChildVerticalAlignmentStrings[] = 
    { { "ALIGNMENT-BASELINE-TOP",    XmALIGNMENT_BASELINE_TOP },
      { "ALIGNMENT-CENTER",          XmALIGNMENT_CENTER },
      { "ALIGNMENT-BASELINE-BOTTOM", XmALIGNMENT_BASELINE_BOTTOM },
      { "ALIGNMENT-WIDGET-TOP",      XmALIGNMENT_WIDGET_TOP },
      { "ALIGNMENT-WIDGET-BOTTOM",   XmALIGNMENT_WIDGET_BOTTOM } };

DEF_ENUM_CONVERTER(ChildVerticalAlignment)

static StringEnum SelectionTypeStrings[] = 
    { { "DIALOG-WORK-AREA",      XmDIALOG_WORK_AREA },
      { "DIALOG-PROMPT",         XmDIALOG_PROMPT },
      { "DIALOG-SELECTION",      XmDIALOG_SELECTION },
      { "DIALOG-COMMAND",        XmDIALOG_COMMAND },
      { "DIALOG-FILE-SELECTION", XmDIALOG_FILE_SELECTION } };

DEF_ENUM_CONVERTER(SelectionType)

static StringEnum TearOffModelStrings[] = 
    { { "TEAR-OFF-ENABLED",  XmTEAR_OFF_ENABLED },
      { "TEAR-OFF-DISABLED", XmTEAR_OFF_DISABLED } };

DEF_ENUM_CONVERTER(TearOffModel)

static StringEnum VerticalAlignmentStrings[] = 
    { { "ALIGNMENT-BASELINE-TOP",    XmALIGNMENT_BASELINE_TOP },
      { "ALIGNMENT-CENTER",          XmALIGNMENT_CENTER },
      { "ALIGNMENT-BASELINE-BOTTOM", XmALIGNMENT_BASELINE_BOTTOM },
      { "ALIGNMENT-CONTENTS-TOP",    XmALIGNMENT_CONTENTS_TOP },
      { "ALIGNMENT-CONTENTS-BOTTOM", XmALIGNMENT_CONTENTS_BOTTOM } };

DEF_ENUM_CONVERTER(VerticalAlignment)

#endif

/* Template
static StringEnum Strings[] = 
    { { "", Xm },
      { "", Xm },
      { "", Xm } };

DEF_ENUM_CONVERTER()

*/










