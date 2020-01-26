static char sccsid[] = "@(#)TypeTable.c	1.9 1/13/94";

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

#include <Xm/Xm.h>
#include <X11/IntrinsicP.h>
#include <X11/StringDefs.h>
#include <Xm/Text.h>
#include <Xm/DialogSP.h>

#ifdef GRAPHWIDGET
#include <Xbab/Graph.h>
#endif GRAPHWIDGET

#include "interface.h"
#include "functions.h"

#ifdef GEN_TABLE
#include "lisp.h"
#endif

/* Routines for converting external rep. to Toolkit rep. */

#ifndef GEN_TABLE
extern caddr_t ClmPCvtInteger(), ClmPCvtString(),    ClmPCvtFontList(), 
    ClmPCvtShort(), ClmPCvtEditMode(), ClmPCvtDeleteResponse(),
    ClmPCvtBoolean(), ClmPCvtPixel(),     ClmPCvtAttachment(),
    ClmPCvtPixmap(),  ClmPCvtWidget(),    ClmPCvtTrans(),  
    ClmPCvtLabelType(), ClmPCvtXmString(), ClmPCvtPacking(),
    ClmPCvtFloat(),   ClmPCvtDimension(), ClmPCvtUChar(),
    ClmPCvtChar(), ClmPCvtPosition(),
    ClmPCvtSeparatorType(), ClmPCvtTextPosition(),
    ClmPCvtDialogStyle(), ClmPCvtScrollBarDisplayPolicy(),
    ClmPCvtScrollBarPlacement(), ClmPCvtScrollingPolicy(),
    ClmPCvtVisualPolicy(), ClmPCvtAlignment(),
    ClmPCvtUnitType(), ClmPCvtListSizePolicy(),
    ClmPCvtResizePolicy(), ClmPCvtArrowDirection(),
    ClmPCvtDefaultButtonType(), ClmPCvtProcessingDirection(),
    ClmPCvtSelectionPolicy(), ClmPCvtIndicatorType(),
    ClmPCvtKeyboardFocusPolicy(), ClmPCvtDialogType(),
    ClmPCvtShadowType(), ClmPCvtStringDirection(),
#ifdef GRAPHWIDGET
    ClmPCvtAlgorithm(), ClmPCvtNodeType(), ClmPCvtEdgeMode(),
#endif GRAPHWIDGET
    ClmPCvtOrientation(), ClmPCvtRowColumnType(),
    ClmPCvtAtom(), ClmPCvtFileTypeMask(), ClmPCvtCommandWindowLocation(),
    ClmPCvtInitialState(), ClmPCvtMultiClick(), ClmPCvtNavigationType(),
    ClmPCvtKeysym();

#if XmREVISION >= 2
extern caddr_t ClmPCvtChildHorizontalAlignment(), ClmPCvtChildPlacement(),
    ClmPCvtChildType(), ClmPCvtChildVerticalAlignment(),
    ClmPCvtSelectionType(), ClmPCvtTearOffModel(), ClmPCvtVerticalAlignment();
#endif
  

/* Routines for converting Toolkit rep. to external rep. */

extern void ClmTCvtBoolean(), ClmTCvtInteger(), ClmTCvtString(),
    ClmTCvtShort(), ClmTCvtBool(), ClmTCvtPosition(),
    ClmTCvtPixel(),   ClmTCvtPixmap(),   ClmTCvtXmString(),
    ClmTCvtFloat(),   ClmTCvtDimension(), ClmTCvtWidget(),
    ClmTCvtScrollBarDisplayPolicy(), ClmTCvtScrollingPolicy(),
    ClmTCvtVisualPolicy(), ClmTCvtOrientation(), ClmTCvtTextPosition(),
    ClmTCvtUChar(), ClmTCvtScrollBarPlacement(), ClmTCvtCommandWindowLocation(),
    ClmTCvtFileTypeMask(), ClmTCvtInitialState(), ClmTCvtMultiClick(),
    ClmTCvtNavigationType(), ClmTCvtLabelType(), ClmTCvtDefaultButtonType(),
    ClmTCvtDialogType(), ClmTCvtArrowDirection(), ClmTCvtUnitType(),
    ClmTCvtDeleteResponse(), ClmTCvtKeyboardFocusPolicy(),
    ClmTCvtSeparatorType(), ClmTCvtEditMode(), ClmTCvtIndicatorType(),
    ClmTCvtShadowType(), ClmTCvtUnitType(), ClmTCvtRowColumnType(),
    ClmTCvtResizePolicy(), ClmTCvtDialogStyle(), ClmTCvtStringDirection(),
    ClmTCvtSelectionPolicy(), ClmTCvtListSizePolicy(),
    ClmTCvtScrollBarPlacement(),ClmTCvtOrientation(), ClmTCvtAlignment(),
    ClmTCvtPacking(), ClmTCvtProcessingDirection(), ClmTCvtAttachment(),
    ClmTCvtKeysym(), ClmTCvtCursor(), ClmTCvtFont(), ClmTCvtColormap(),
    ClmTCvtAtom(), ClmTCvtScreen(), ClmTCvtVisual();

#ifdef GRAPH_WIDGET
extern void ClmTCvtAlgorithm(), ClmTCvtNodeType(), ClmTCvtEdgeMode(), 
#endif

#if XmREVISION >= 2
extern void ClmTCvtChildHorizontalAlignment(), ClmTCvtChildPlacement(),
    ClmTCvtChildType(), ClmTCvtChildVerticalAlignment(),
    ClmTCvtSelectionType(), ClmTCvtTearOffModel(), ClmTCvtVerticalAlignment();
#endif
#endif

#ifdef GEN_TABLE
#define TYPE(name, p_cvt, l_cvt, type) { name, NULL, NULL, 0 },
#else
#define TYPE(name, p_cvt, l_cvt, type) { name, p_cvt, l_cvt, type },
#endif

ClmResourceType TypeTable[] = {
#if XmREVISION != 0
 TYPE(XtRAtom, ClmPCvtAtom, ClmTCvtAtom, ClmArgString)
 TYPE(XtRBitmap, ClmPCvtPixmap, ClmTCvtPixmap, ClmArgString)
 TYPE(XmRBooleanDimension, ClmPCvtDimension, ClmTCvtDimension, ClmArgInteger)
 TYPE(XtRCardinal, ClmPCvtDimension, ClmTCvtDimension, ClmArgInteger)
 TYPE(XtRColormap, NULL, ClmTCvtColormap, ClmArgInteger)
 TYPE(XmRCommandWindowLocation, ClmPCvtCommandWindowLocation, ClmTCvtCommandWindowLocation,ClmArgSymbol)
 TYPE(XmRFileTypeMask, ClmPCvtFileTypeMask, ClmTCvtFileTypeMask, ClmArgSymbol)
 TYPE(XmRGadgetPixmap, ClmPCvtPixmap, ClmTCvtPixmap, ClmArgString)
 TYPE(XmRHorizontalDimension,ClmPCvtDimension,ClmTCvtDimension, ClmArgInteger)
 TYPE(XmRHorizontalInt, ClmPCvtInteger, ClmTCvtInteger, ClmArgInteger)
 TYPE(XmRHorizontalPosition, ClmPCvtPosition, ClmTCvtPosition, ClmArgInteger)
 TYPE(XtRInitialState, ClmPCvtInitialState, ClmTCvtInitialState, ClmArgSymbol)
 TYPE(XmRKeySym, ClmPCvtKeysym, ClmTCvtKeysym, ClmArgString)
 TYPE(XmRMultiClick, ClmPCvtMultiClick, ClmTCvtMultiClick, ClmArgSymbol)
 TYPE(XmRNavigationType, ClmPCvtNavigationType, ClmTCvtNavigationType, ClmArgSymbol)
 TYPE(XtRScreen, NULL, ClmTCvtScreen, ClmArgInteger)
 TYPE(XmRShellHorizDim,ClmPCvtDimension,ClmTCvtDimension, ClmArgInteger)
 TYPE(XmRShellHorizPos, ClmPCvtPosition, ClmTCvtPosition, ClmArgInteger)
 TYPE(XmRShellVertDim,ClmPCvtDimension,ClmTCvtDimension, ClmArgInteger)
 TYPE(XmRShellVertPos, ClmPCvtPosition, ClmTCvtPosition, ClmArgInteger)
 TYPE(XtRStringArray, NULL, NULL, 0)
 TYPE(XmRTextPosition, ClmPCvtTextPosition,ClmTCvtTextPosition, ClmArgInteger)
 TYPE(XmRVerticalDimension,ClmPCvtDimension,ClmTCvtDimension, ClmArgInteger)
 TYPE(XmRVerticalInt, ClmPCvtInteger, ClmTCvtInteger, ClmArgInteger)
 TYPE(XmRVerticalPosition, ClmPCvtPosition, ClmTCvtPosition, ClmArgInteger)
 TYPE(XtRVisual, NULL, ClmTCvtVisual, ClmArgInteger)
 TYPE(XmRWidget, ClmPCvtWidget, ClmTCvtWidget, ClmArgInteger)
 TYPE(XmRWidgetList, NULL, NULL, 0)
#endif
 TYPE("XmBackgroundPixmap", ClmPCvtPixmap, ClmTCvtPixmap, ClmArgString)
    
 TYPE(XmRPrimForegroundPixmap, ClmPCvtPixmap, ClmTCvtPixmap, ClmArgString)
 TYPE(XmRPrimHighlightPixmap, ClmPCvtPixmap, ClmTCvtPixmap, ClmArgString)
 TYPE(XmRPrimTopShadowPixmap, ClmPCvtPixmap, ClmTCvtPixmap, ClmArgString)
 TYPE(XmRPrimBottomShadowPixmap, ClmPCvtPixmap, ClmTCvtPixmap, ClmArgString)
 TYPE(XmRManForegroundPixmap, ClmPCvtPixmap, ClmTCvtPixmap, ClmArgString)
 TYPE(XmRManBottomShadowPixmap, ClmPCvtPixmap, ClmTCvtPixmap, ClmArgString)
 TYPE(XmRManBottomShadowPixmap, ClmPCvtPixmap, ClmTCvtPixmap, ClmArgString)
 TYPE(XmRManTopShadowPixmap, ClmPCvtPixmap, ClmTCvtPixmap, ClmArgString)
 TYPE(XmRManHighlightPixmap, ClmPCvtPixmap, ClmTCvtPixmap, ClmArgString)
 TYPE(XmRLabelType, ClmPCvtLabelType, ClmTCvtLabelType, ClmArgSymbol)
 TYPE(XmRXmString, ClmPCvtXmString, ClmTCvtXmString, ClmArgString)
 TYPE(XmRProc, NULL, NULL, 0)
 TYPE(XmRDefaultButtonType, ClmPCvtDefaultButtonType, ClmTCvtDefaultButtonType, ClmArgSymbol)
 TYPE(XmRDialogType, ClmPCvtDialogType, ClmTCvtDialogType, ClmArgSymbol)
 TYPE(XmRArrowDirection, ClmPCvtArrowDirection, ClmTCvtArrowDirection, ClmArgSymbol)
 TYPE(XmRShellUnitType, ClmPCvtUnitType, ClmTCvtUnitType, ClmArgSymbol)
 TYPE(XmRDeleteResponse, ClmPCvtDeleteResponse, ClmTCvtDeleteResponse, ClmArgSymbol)
 TYPE(XmRKeyboardFocusPolicy, ClmPCvtKeyboardFocusPolicy, ClmTCvtKeyboardFocusPolicy, ClmArgSymbol)
 TYPE(XmRSeparatorType, ClmPCvtSeparatorType, ClmTCvtSeparatorType, ClmArgSymbol)
 TYPE(XmREditMode, ClmPCvtEditMode, ClmTCvtEditMode, ClmArgSymbol)
 TYPE(XmRIndicatorType, ClmPCvtIndicatorType, ClmTCvtIndicatorType, ClmArgSymbol)
 TYPE(XmRXmStringTable, NULL, NULL, 0)
 TYPE(XmRShadowType, ClmPCvtShadowType, ClmTCvtShadowType, ClmArgSymbol)
 TYPE(XmRUnitType, ClmPCvtUnitType, ClmTCvtUnitType, ClmArgSymbol)
 TYPE(XmRResizePolicy, ClmPCvtResizePolicy, ClmTCvtResizePolicy, ClmArgSymbol)
 TYPE(XmRDialogStyle, ClmPCvtDialogStyle, ClmTCvtDialogStyle, ClmArgSymbol)
 TYPE(XmRStringDirection, ClmPCvtStringDirection, ClmTCvtStringDirection, ClmArgSymbol)
 TYPE(XmRSelectionPolicy, ClmPCvtSelectionPolicy, ClmTCvtSelectionPolicy, ClmArgSymbol)
 TYPE(XmRListSizePolicy, ClmPCvtListSizePolicy, ClmTCvtListSizePolicy, ClmArgSymbol)
 TYPE(XmRScrollBarDisplayPolicy, ClmPCvtScrollBarDisplayPolicy, ClmTCvtScrollBarDisplayPolicy, ClmArgSymbol)
 TYPE(XmRScrollingPolicy, ClmPCvtScrollingPolicy, ClmTCvtScrollingPolicy, ClmArgSymbol)
 TYPE(XmRVisualPolicy, ClmPCvtVisualPolicy, ClmTCvtVisualPolicy, ClmArgSymbol)
 TYPE(XmRScrollBarPlacement, ClmPCvtScrollBarPlacement, ClmTCvtScrollBarPlacement, ClmArgSymbol)
 TYPE(XmRWhichButton, ClmPCvtInteger, ClmTCvtInteger, ClmArgInteger)
 TYPE(XmROrientation, ClmPCvtOrientation, ClmTCvtOrientation, ClmArgSymbol)
 TYPE(XmRAlignment, ClmPCvtAlignment, ClmTCvtAlignment, ClmArgSymbol)
 TYPE(XmRPacking, ClmPCvtPacking, ClmTCvtPacking, ClmArgSymbol)
 TYPE(XmRRowColumnType, ClmPCvtRowColumnType, ClmTCvtRowColumnType, ClmArgSymbol )
 TYPE(XmRChar, ClmPCvtChar, NULL, ClmArgString)
 TYPE(XmRProcessingDirection, ClmPCvtProcessingDirection, ClmTCvtProcessingDirection, ClmArgSymbol)
 TYPE(XmRAttachment, ClmPCvtAttachment, ClmTCvtAttachment, ClmArgSymbol)
 TYPE(XmRMenuWidget, ClmPCvtWidget, ClmTCvtWidget, ClmArgInteger)
 TYPE(XtRAcceleratorTable, ClmPCvtTrans, NULL, ClmArgString)
 TYPE(XtRBool, ClmPCvtBoolean, ClmTCvtBool, ClmArgSymbol)
 TYPE(XtRBoolean, ClmPCvtBoolean, ClmTCvtBoolean, ClmArgSymbol)
 TYPE(XtRCallback, NULL, NULL, 0 )
 TYPE(XtRCallProc, NULL, NULL, 0 )
 TYPE(XtRColor, NULL, NULL, 0 )
 TYPE(XtRDimension, ClmPCvtDimension, ClmTCvtDimension, ClmArgInteger)
 TYPE(XtRDisplay, NULL, NULL, 0 )
 TYPE(XmRFontList, ClmPCvtFontList, NULL, ClmArgString)
 TYPE(XtRFontStruct, NULL, NULL, 0 )
 TYPE(XtRFunction, NULL, NULL, 0 )
 TYPE(XtRGeometry, NULL, NULL, 0 )
 TYPE(XtRImmediate, NULL, NULL, 0 )
 TYPE(XtRInt, ClmPCvtInteger, ClmTCvtInteger, ClmArgInteger)
 TYPE(XtRLongBoolean, ClmPCvtBoolean, NULL, ClmArgSymbol)
 TYPE(XtRPixel, ClmPCvtPixel, ClmTCvtPixel, ClmArgString)
 TYPE(XtRPixmap, ClmPCvtPixmap, ClmTCvtPixmap, ClmArgString)
 TYPE(XtRPointer, NULL, NULL, 0 )
 TYPE(XtRPosition, ClmPCvtPosition, ClmTCvtPosition, ClmArgInteger)
 TYPE(XtRShort, ClmPCvtShort, ClmTCvtShort, ClmArgInteger)
 TYPE(XtRString, ClmPCvtString, ClmTCvtString, ClmArgString)
 TYPE(XtRStringTable, NULL, NULL, 0 )
 TYPE(XtRUnsignedChar, ClmPCvtUChar, ClmTCvtUChar, ClmArgString)
 TYPE(XtRTranslationTable, ClmPCvtTrans, NULL, ClmArgString)
 TYPE(XtRWindow, ClmPCvtWidget, ClmTCvtWidget, ClmArgInteger)
#ifdef GRAPHWIDGET
 TYPE(XmRAlgorithm, ClmPCvtAlgorithm, ClmTCvtAlgorithm, ClmArgSymbol)
 TYPE(XmRNodeType, ClmPCvtNodeType, ClmTCvtNodeType, ClmArgSymbol)
 TYPE(XmREdgeMode, ClmPCvtEdgeMode, ClmTCvtEdgeMode, ClmArgSymbol)
#endif GRAPHWIDGET
 TYPE(XtRCursor, NULL, ClmTCvtCursor, ClmArgInteger)
 TYPE(XtRFont, NULL, ClmTCvtFont, ClmArgInteger)
#if XmREVISION >= 2
 TYPE(XmRChildHorizontalAlignment, ClmPCvtChildHorizontalAlignment, ClmTCvtChildHorizontalAlignment, ClmArgSymbol)
 TYPE(XmRChildPlacement, ClmPCvtChildPlacement, ClmTCvtChildPlacement, ClmArgSymbol)
 TYPE(XmRChildType, ClmPCvtChildType, ClmTCvtChildType, ClmArgSymbol)
 TYPE(XmRChildVerticalAlignment, ClmPCvtChildVerticalAlignment, ClmTCvtChildVerticalAlignment, ClmArgSymbol)
 TYPE(XmRSelectionType, ClmPCvtSelectionType, ClmTCvtSelectionType, ClmArgSymbol)
 TYPE(XmRTearOffModel, ClmPCvtTearOffModel, ClmTCvtTearOffModel, ClmArgSymbol)
 TYPE(XmRVerticalAlignment, ClmPCvtVerticalAlignment, ClmTCvtVerticalAlignment, ClmArgSymbol)
 TYPE(XmRWidgetClass, NULL, NULL, ClmArgSymbol)
 TYPE(XmRValueWcs, NULL, NULL, ClmArgString)
#ifdef XmRTopItemPosition
 TYPE(XmRTopItemPosition, ClmPCvtInteger, ClmTCvtInteger, ClmArgInteger)
#endif
#endif

 {NULL, NULL, NULL, 0}
};
#ifdef GEN_TABLE
LispResource LispTypeTable[] = {
 {XmRPrimForegroundPixmap,   "pixmap", "pixmap"},
 {XmRPrimHighlightPixmap,    "pixmap", "pixmap"},
 {XmRPrimTopShadowPixmap,    "pixmap", "pixmap"},
 {XmRPrimBottomShadowPixmap, "pixmap", "pixmap"},
 {XmRManForegroundPixmap,    "pixmap", "pixmap"},
 {XmRManBottomShadowPixmap,  "pixmap", "pixmap"},
 {XmRManBottomShadowPixmap,  "pixmap", "pixmap"},
 {XmRManTopShadowPixmap,     "pixmap", "pixmap"},
 {XmRManHighlightPixmap,     "pixmap", "pixmap"},
 {XmRLabelType,              "enum", "label-type"},
 {XmRXmString,               "xm-string", "xm-string"},
 {XmRProc,                   "nil", "nil"},
 {XmRDefaultButtonType,      "enum", "default-button-type"},
 {XmRDialogType,             "enum", "dialog-type"},
 {XmRArrowDirection,         "enum", "arrow-direction"},
 {XmRShellUnitType,          "enum", "shell-unit-type"},
 {XmRDeleteResponse,         "enum", "delete-response"},
 {XmRKeyboardFocusPolicy,    "enum", "keyboard-focus-policy"},
 {XmRSeparatorType,          "enum", "separator-type"},
 {XmREditMode,               "enum", "edit-mode"},
 {XmRIndicatorType,          "enum", "indicator-type"},
 {XmRXmStringTable,          "xm-string-table", "xm-string-table"},
 {XmRShadowType,             "enum", "shadow-type"},
 {XmRUnitType,               "enum", "unit-type"},
 {XmRResizePolicy,           "enum", "resize-policy"},
 {XmRDialogStyle,            "enum", "dialog-style"},
 {XmRStringDirection,        "enum", "string-direction"},
 {XmRSelectionPolicy,        "enum", "selection-policy"},
 {XmRListSizePolicy,         "enum", "list-size-policy"},
 {XmRScrollBarDisplayPolicy, "enum", "scroll-bar-display-policy"},
 {XmRScrollingPolicy,        "enum", "scrolling-policy"},
 {XmRVisualPolicy,           "enum", "visual-policy"},
 {XmRScrollBarPlacement,     "enum", "scroll-bar-placement"},
 {XmRWhichButton,            "int", "int"},
 {XmROrientation,            "enum", "orientation"},
 {XmRAlignment,              "enum", "alignment"},
 {XmRPacking,                "enum", "packing"},
 {XmRRowColumnType,          "enum", "row-column-type"},
 {XmRChar,                   "char", "char"},
 {XmRProcessingDirection,    "enum", "processing-direction"},
 {XmRAttachment,             "enum", "attachment"},
 {XmRMenuWidget,             "widget", "widget"},
 {XtRAcceleratorTable,       "translation", "translation"},
 {XtRBool,                   "bool", "bool"},
 {XtRBoolean,                "boolean", "boolean"},
 {XtRCallback,               "nil", "nil"},
 {XtRCallProc,               "nil", "nil"},
 {XtRColor,                  "nil", "nil"},
 {XtRDimension,              "dimension", "dimension"},
 {XtRDisplay,                "nil", "nil"},
 {XmRFontList,               "font-list", "font-list"},
 {XtRFontStruct,             "nil", "nil"},
 {XtRFunction,               "nil", "nil"},
 {XtRGeometry,               "nil", "nil"},
 {XtRImmediate,              "nil", "nil"},
 {XtRInt,                    "int", "int"},
 {XtRLongBoolean,            "long-boolean", "long-boolean"},
 {XtRPixel,                  "pixel", "nil"},
 {XtRPixmap,                 "pixmap", "pixmap"},
 {XtRPointer,                "nil", "nil"},
 {XtRPosition,               "Position", "Position"},
 {XtRShort,                  "short", "short"},
 {XtRString,                 "string", "string"},
 {XtRStringTable,            "string-table", "string-table"},
 {XtRUnsignedChar,           "char", "char"},
 {XtRTranslationTable,       "translation", "translation"},
 {XtRWindow,                 "widget", "widget"},
#ifdef GRAPHWIDGET
 {XmRAlgorithm,              "enum", "algorithm"},
 {XmRNodeType,               "enum", "node-type"},
 {XmREdgeMode,               "enum", "edge-mode"},
#endif GRAPHWIDGET
 {NULL, NULL, NULL},
};
#endif
