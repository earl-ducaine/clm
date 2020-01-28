/* "@(#)opcodes.h	1.8 9/21/93" */

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

/* Values for the ClmCommand.command field */

#define ClmClose                 1    /* Close connection to the Toolkit */
#define ClmCreate                2    /* Create an unmanaged widget */
#define ClmCreateManaged         3    /* Create a managed widget */
#define ClmDestroy               4    /* destroy a widget */
#define ClmSetValues             5    /* Set Values */
#define ClmGetValues             6    /* Get Values */
#define ClmCreateApplication     7    /* Create a shell Widget */
#define ClmRealize               8    /* Realize a widget tree */
#define ClmAddCallback           9    /* add a callback to a list */
#define ClmRemoveCallback       10    /* Remove a single callback */
#define ClmChangeCursor         11
#define ClmDefineCursor         12
#define ClmSetManaged           13    /* set/unset managed status for widget */
#define ClmDestroyApplication   14
#define ClmSetMap               15    /* set/unset mapped status for widget */
#define ClmIsManaged            16
#define ClmSetSensitivity       17    /* change sensitivity status of a widget*/
#define ClmCreatePopupShell     18    /* create a Popup shell */
#define ClmPopup                19    /* Pop up a Shell widget */
#define ClmPopdown              20    /* Pop down a Shell widget */
#define ClmTranslateCoords      21    /* Translate widget coords */
#define ClmMove                 22    /* Move widget */
#define ClmResize               23    /* Resize Widget */
#define ClmConfigure            24    /* Configure Widget */
#define ClmAugmentTranslations  25    /* Merge translation tables */
#define ClmOverrideTranslations 26    /* Override translation table */
#define ClmEvent                27    /* Event message */
#define ClmConfirm              28    /* Confirm a Command */
#define ClmTerminate            29    /* Terminate ClmLoop() */
#define ClmMainLoop             30    /* Enter XtMainLoop() */
#define ClmReturnValues         31    /* Send values to prolab */
#define ClmAddEventHandler      32
#define ClmRemoveEventHandler   33
#define ClmGetScreenSize        34
#define ClmManagePopupChild     35
#define ClmUnmanagePopupChild   36
#define ClmGetShellClasses      37
#define ClmGetCompositeClasses  38
#define ClmGetSimpleClasses     39
#define ClmGetResources         40
#define ClmAddTabGroup          41
#define ClmRemoveTabGroup       42
#define ClmGetWindowID          43
#define ClmGetParent            44
#define ClmListSetItems         45
#define ClmListAddItem          46
#define ClmListAddItemUnselected 47
#define ClmListDeleteItem        48
#define ClmListDeletePos         49
#define ClmListDeselectItem      50
#define ClmListDeselectAllItems  51
#define ClmListSelectItem        52
#define ClmListSetHorizPos       53
#define ClmListSetItem           54
#define ClmListSetPos            55
#define ClmListSetBottomItem     56
#define ClmListSetBottomPos      57
#define ClmListSelectPos         58
#define ClmListDeselectPos       59
#define ClmListItemExists        60
#define ClmListGetItems          61
#define ClmTextInsert            62
#define ClmProtocolAddCallback   63
#define ClmProtocolRemoveCallback 64
#define ClmExecShellCmd           65
#define ClmQueryCursor            66
#define ClmPushTranslations 67
#define ClmPopTranslations 68
#define ClmShutdown 69
#define ClmGetTextSelection 70
#define ClmIdentifyClient 71
#define ClmRaiseWindow 72
#define ClmForcedOutputMode 73
#define ClmIsMwmRunning 74
#define ClmConvenience 75
#define ClmUpdateDisplay 76
#define ClmCreatePixmapCursor 77

#ifdef GRAPHWIDGET
#define ClmAddGraphRelations 78
#define ClmRemoveGraphRelations 79
#define ClmDoLayout 80
#endif // GRAPHWIDGET

#define ClmCreateTimer 81
#define ClmDestroyTimer 82
#define ClmChangeTimer 83
#define ClmRestartTimer 84
#define ClmStopTimer 85

/* Definitions added for Motif 1.1 */
#define ClmCascadeButtonGadgetHighlight 86
#define ClmCascadeButtonHighlight 87
#define ClmCommandAppendValue 88
#define ClmCommandError 89
#define ClmCommandGetChild 90
#define ClmCommandSetValue 91
#define ClmDestroyPixmap 92
#define ClmFileSelectionBoxGetChild 93
#define ClmFileSelectionDoSearch 94
#define ClmGetColors 95
#define ClmGetDestination 96
#define ClmGetPostedFromWidget 97
#define ClmMenuPosition 98
#define ClmMessageBoxGetChild 99
#define ClmOptionButtonGadget 100
#define ClmOptionLabelGadget 101
#define ClmProcessTraversal 102
#define ClmScaleGetValue 103
#define ClmScaleSetValue 104
#define ClmScrollBarGetValues 105
#define ClmScrollBarSetValues 106
#define ClmScrolledWindowSetAreas 107
#define ClmSelectionBoxGetChild 108
#define ClmSetMenuCursor 109
/* The following functions also apply to XmTextField widgets */
#define ClmTextClearSelection 110
#define ClmTextCopy 111
#define ClmTextCut 112
#define ClmTextGetBaseline 113
#define ClmTextGetEditable 114
#define ClmTextGetInsertionPosition 115
#define ClmTextGetLastPosition 116
#define ClmTextGetMaxLength 117
#define ClmTextGetSelection 118
#define ClmTextGetSelectionPosition 119
#define ClmTextGetString 120
#define ClmTextFieldInsert 121
#define ClmTextPaste 122
#define ClmTextPosToXY 123
#define ClmTextRemove 124
#define ClmTextReplace 125
#define ClmTextSetAddMode 126
#define ClmTextSetEditable 127
#define ClmTextSetHighlight 128
#define ClmTextSetInsertionPosition 129
#define ClmTextSetMaxLength 130
#define ClmTextSetSelection 131
#define ClmTextSetString 132
#define ClmTextShowPosition 133
#define ClmTextXYToPos 134
/* These are only for Text */
#define ClmTextGetSource 135
#define ClmTextGetTopCharacter 136
#define ClmTextScroll 137
#define ClmTextSetSource 138
#define ClmTextSetTopCharacter 139
#define ClmToggleButtonGetState 140
#define ClmToggleButtonSetState 141
#define ClmTrackingLocate 142

#define ClmIsRealized 143
#define ClmGetMultiClickTime 144
#define ClmLastTimestampProcessed 145
#define ClmIsValidWidgetID 146
#define ClmWidgetFullName 147
#define ClmWidgetFullClass 148

/* Misc */
#define ClmRunStatus 149
#define ClmMainWindowSetAreas 150
#define ClmTextGetSubstring 151
#define ClmTextSearch 152
#define ClmEnableWithoutMouse 153
#define ClmDisableWithoutMouse 154

#define ClmMaxID 154

/* Minor opcodes for ClmEvent */

#define ClmEventCallback    0
#define ClmEventEvent       1
#define ClmEventProtocol    2
#define ClmEventDestroy     3
#define ClmEventAction      4
#define ClmEventTimer       5
#define ClmEventSimpleTimer 6
#define ClmEventError       7
#define ClmEventWarning     8
#define ClmEventMainLoop    9

/* Minor opcodes for ClmConfirm */

#define ClmConfirmConfirm 0
#define ClmConfirmResult  1
#define ClmConfirmError   2
