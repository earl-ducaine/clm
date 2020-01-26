static char sccsid[] = "@(#)main.c	1.8 9/21/93";

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
#include <X11/Shell.h>
#include <Xm/Xm.h>

#define CLM_MAIN_PROGRAM

#include "interface.h"
#include "functions.h"
#include "opcodes.h"

ClmGlobal global;

extern char *getenv();

/* Functions from Traversal.c */
extern ClmCommand *ClmFprocessTraversal();

/* Functions from Convenience.c */
extern ClmCommand *ClmFconvenience();

/* Functions from DisplayMgr.c */
extern ClmCommand *ClmFupdateDisplay();

/* Functions from WidgetMgr.c */
extern ClmCommand *ClmFcreate(), *ClmFdestroy(), 
                  *ClmFrealize(), *ClmFgetWindowID(), *ClmFgetParent(),
                  *ClmFsetManaged(), *ClmFsetMap(), *ClmFsetSensitivity(),
                  *ClmFcreatePopupShell(), *ClmFraiseWindow();

/* Functions from AppMgr.c */
extern ClmCommand *ClmFcreateApplication(), *ClmFdestroyApplication(), 
                  *ClmFterminate(), *ClmFmainLoop(), *ClmFshutdown(),
                  *ClmFclose(), *ClmFforcedOutputMode();

/* Functions from ResourceMgr.c */
extern ClmCommand *ClmFsetValues(), *ClmFgetValues(), *ClmFgetResources();

/* Functions from PopupMgr.c */
extern ClmCommand *ClmFpopup(), *ClmFpopdown(), *ClmFmanagePopupChild(), 
                  *ClmFunmanagePopupChild();

/* Functions from TranslationMgr.c */
extern ClmCommand *ClmFaugmentTranslations(), *ClmFoverrideTranslations(),
                  *ClmFaddTabGroup(), *ClmFremoveTabGroup(),
                  *ClmFpushTranslations(), *ClmFpopTranslations();

/* Functions from GeometryMgr.c */
extern ClmCommand *ClmFmove(), *ClmFresize(), *ClmFconfigure(), 
                  *ClmFtranslateCoords(), *ClmFgetScreenSize();

/* Functions from EventMgr.c */
extern ClmCommand *ClmFaddEventHandler(), *ClmFremoveEventHandler();
extern ClmCommand *ClmFlastTimestampProcessed();

/* Functions from CallbackMgr.c */
extern ClmCommand *ClmFaddCallback(),   *ClmFremoveCallback();

/* Functions from ProtocolMgr.c */
extern ClmCommand *ClmFprotocolAddCallback(), *ClmFprotocolRemoveCallback();

/* Functions from List.c */
extern ClmCommand *ClmFlistSetItems(), *ClmFlistGetItems(), *ClmFlistAddItem(),
                  *ClmFlistAddItemUnselected(), *ClmFlistDeleteItem(),
                  *ClmFlistDeletePos(), *ClmFlistDeselectItem(),
                  *ClmFlistDeselectAllItems(), *ClmFlistSelectItem(),
                  *ClmFlistSetHorizPos(), *ClmFlistSetItem(), 
                  *ClmFlistSetPos(), *ClmFlistSetBottomItem(), 
                  *ClmFlistSetBottomPos(), *ClmFlistSelectPos(),
                  *ClmFlistDeselectPos();

/* Functions from Text.c */

extern ClmCommand *ClmFtextInsert(),
                  *ClmFtextClearSelection(), *ClmFtextCut(), *ClmFtextCopy(),
                  *ClmFtextGetBaseline(), *ClmFtextGetSelection(),
                  *ClmFtextGetSelectionPosition(), *ClmFtextPaste(),
                  *ClmFtextPosToXY(), *ClmFtextRemove(), *ClmFtextScroll(),
                  *ClmFtextSetAddMode(), *ClmFtextShowPosition(),
                  *ClmFtextXYToPos();

/* Functions from ShellCmd.c */

extern ClmCommand *ClmFexecShellCmd();

/* Functions from Cursor.c */

extern ClmCommand *ClmFqueryCursor();
extern ClmCommand *ClmFchangeCursor();
extern ClmCommand *ClmFdefineCursor();
extern ClmCommand *ClmFcreateCursor();

#ifdef GRAPHWIDGET
/* Functions from Graph.c */

extern ClmCommand *ClmFdoLayout();
extern ClmCommand *ClmFaddGraphRelations();
extern ClmCommand *ClmFremoveGraphRelations();
#endif GRAPHWIDGET

/* Functions from Mwm.c */

extern ClmCommand *ClmFisMwmRunning();

/* Functions from Timer.c */

extern ClmCommand *ClmFcreateTimer(), *ClmFdestroyTimer(),
                  *ClmFchangeTimer(), *ClmFrestartTimer(),
                  *ClmFstopTimer();

/* Functions from MotifMisc.c */

extern ClmCommand *ClmFselectionBoxGetChild(),
                  *ClmFcommandGetChild(),
		  *ClmFfileSelectionBoxGetChild(),
		  *ClmFmessageBoxGetChild(),
                  *ClmFoptionButtonGadget(),
                  *ClmFoptionLabelGadget(),
		  *ClmFmainWindowSetAreas(),
		  *ClmFscrolledWindowSetAreas(),
		  *ClmFisRealized(),
                  *ClmFisManaged(),
		  *ClmFgetMultiClickTime(),
		  *ClmFisValidWidgetID(),
		  *ClmFwidgetFullName(),
		  *ClmFwidgetFullClass(),
                  *ClmFrunStatus(),
                  *ClmFcascadeButtonHighlight(),
                  *ClmFenableWithoutMouse(),
                  *ClmFdisableWithoutMouse();

/* Functions from Text2.c */

extern ClmCommand *ClmFtextGetSubstring(), *ClmFtextSearch(),
                  *ClmFtextGetLastPosition(), *ClmFtextSetSelection(),
                  *ClmFtextReplace(), *ClmFtextGetInsertionPosition(),
                  *ClmFtextSetHighlight();


static ClmCommand *(*FunctionTable[])() = {
                              NULL,
/* ClmClose                */ ClmFclose,
/* ClmCreate               */ ClmFcreate,
                              NULL,
/* ClmDestroy              */ ClmFdestroy,
/* ClmSetValues            */ ClmFsetValues,
/* ClmGetValues            */ ClmFgetValues,
/* ClmCreateShell          */ ClmFcreateApplication,
/* ClmRealize              */ ClmFrealize,
/* ClmAddCallback          */ ClmFaddCallback,
/* ClmRemoveCallback       */ ClmFremoveCallback,            /* 10 */
			      ClmFchangeCursor,
                              ClmFdefineCursor,
/* ClmSetManaged           */ ClmFsetManaged,
                              ClmFdestroyApplication,
/* ClmSetMap               */ ClmFsetMap,
                              ClmFisManaged,
/* ClmSetSensitivity       */ ClmFsetSensitivity,
/* ClmCreatePopupShell     */ ClmFcreatePopupShell,
/* ClmPopup                */ ClmFpopup,
/* ClmPopdown              */ ClmFpopdown,                   /* 20 */
/* ClmTranslateCoordinates */ ClmFtranslateCoords,
/* ClmMove                 */ ClmFmove,
/* ClmResize               */ ClmFresize,
/* ClmConfigure            */ ClmFconfigure,
/* ClmAugmentTranslations  */ ClmFaugmentTranslations,
/* ClmOverrideTranslations */ ClmFoverrideTranslations,
                              NULL,
                              NULL,
/* ClmTerminate            */ ClmFterminate,
/* ClmMainLoop             */ ClmFmainLoop,                  /* 30 */
                              NULL,
                              ClmFaddEventHandler,
                              ClmFremoveEventHandler,
                              ClmFgetScreenSize,
                              ClmFmanagePopupChild,
                              ClmFunmanagePopupChild,
                              NULL,
                              NULL,
                              NULL,
                              ClmFgetResources,              /* 40 */
                              ClmFaddTabGroup,
                              ClmFremoveTabGroup,
                              ClmFgetWindowID,
                              ClmFgetParent,
                              ClmFlistSetItems,
                              ClmFlistAddItem,
                              ClmFlistAddItemUnselected,
                              ClmFlistDeleteItem,
                              ClmFlistDeletePos,
                              ClmFlistDeselectItem,          /* 50 */
                              ClmFlistDeselectAllItems,
                              ClmFlistSelectItem,
                              ClmFlistSetHorizPos,
                              ClmFlistSetItem,
                              ClmFlistSetPos,
                              ClmFlistSetBottomItem,
                              ClmFlistSetBottomPos,
                              ClmFlistSelectPos,
                              ClmFlistDeselectPos,
                              NULL,                          /* 60 */
                              ClmFlistGetItems,
                              ClmFtextInsert,
                              ClmFprotocolAddCallback,
                              ClmFprotocolRemoveCallback,
                              ClmFexecShellCmd,
                              ClmFqueryCursor,
                              ClmFpushTranslations,
                              ClmFpopTranslations,
                              ClmFshutdown,
                              ClmFtextGetSelection,          /* 70 */
                              NULL,
                              ClmFraiseWindow,
                              ClmFforcedOutputMode,
                              ClmFisMwmRunning,
                              ClmFconvenience,
			      ClmFupdateDisplay,
			      ClmFcreateCursor,
#ifdef GRAPHWIDGET
			      ClmFaddGraphRelations,
			      ClmFremoveGraphRelations,
			      ClmFdoLayout,                  /* 80 */
#else
			      NULL,
			      NULL,
			      NULL,                          /* 80 */
#endif GRAPHWIDGET
                              ClmFcreateTimer,
			      ClmFdestroyTimer,
			      ClmFchangeTimer,
			      ClmFrestartTimer,
			      ClmFstopTimer,
			      NULL,
			      ClmFcascadeButtonHighlight,
			      NULL,
			      NULL,
			      ClmFcommandGetChild,           /* 90 */
			      NULL,
			      NULL,
			      ClmFfileSelectionBoxGetChild,
			      NULL,
			      NULL,
			      NULL,
			      NULL,
			      NULL,
			      ClmFmessageBoxGetChild,
			      ClmFoptionButtonGadget,        /* 100 */
			      ClmFoptionLabelGadget,
			      ClmFprocessTraversal,
			      NULL,
			      NULL,
			      NULL,
			      NULL,
			      ClmFscrolledWindowSetAreas,
			      ClmFselectionBoxGetChild,
			      NULL,
			      ClmFtextClearSelection,        /* 110 */
			      ClmFtextCopy,
			      ClmFtextCut,
			      ClmFtextGetBaseline,
			      NULL,
			      ClmFtextGetInsertionPosition,
			      ClmFtextGetLastPosition,
			      NULL,
			      ClmFtextGetSelection,
			      ClmFtextGetSelectionPosition,
			      NULL,                          /* 120 */
			      NULL,
			      ClmFtextPaste,
			      ClmFtextPosToXY,
			      ClmFtextRemove,
			      ClmFtextReplace,
			      ClmFtextSetAddMode,
			      NULL,
			      ClmFtextSetHighlight,
			      NULL,
			      NULL,                          /* 130 */
			      ClmFtextSetSelection,
			      NULL,
			      ClmFtextShowPosition,
			      ClmFtextXYToPos,
			      NULL,
			      NULL,
			      ClmFtextScroll,
			      NULL,
			      NULL,
			      NULL,                          /* 140 */
			      NULL,
			      NULL,
			      ClmFisRealized,
			      ClmFgetMultiClickTime,
			      ClmFlastTimestampProcessed,
			      ClmFisValidWidgetID,
			      ClmFwidgetFullName,
			      ClmFwidgetFullClass,
			      ClmFrunStatus,
			      ClmFmainWindowSetAreas,        /* 150 */
			      ClmFtextGetSubstring,
			      ClmFtextSearch,
                              ClmFenableWithoutMouse,
                              ClmFdisableWithoutMouse,
};

/* put your private extensions here starting with opcode 1024 */

#define UserMaxID 1024
static ClmCommand *(*UserFunctionTable[])() = {
                              NULL,
};

char *ClmMalloc(n_bytes)
unsigned int n_bytes;
{
    char *ptr;

    if( (ptr = XtMalloc(n_bytes)) == NULL ) {
	fprintf(stderr, "Fatal server error: Out of memory.\n");
	fprintf(stderr, "Failed to allocate %d bytes.\n", n_bytes);
	fflush(stderr);
    }
    return(ptr);
}

void ClmErrorHandler(message)
char *message;
{
    char *em = "Fatal Error: ";

    /* global.error_message = ClmMalloc(strlen(message)+strlen(em)+1);
       strcpy(global.error_message, em);
       strcat(global.error_message, message);
    */
    fprintf(stderr, em);
    fprintf(stderr, message);
    fprintf(stderr, "\n");
    fflush(stderr);
    do_error(message);
    exit (1);
}

void ClmWarningHandler(message)
char *message;
{
    char *wm = "Warning: ";

    global.warning_message = ClmMalloc(strlen(message)+strlen(wm)+1);
    strcpy(global.warning_message, wm);
    strcat(global.warning_message, message);
}

ClmCommand *ReceiveAndExecute()
{
    ClmCommand *cmd, *rc;

    if( (cmd = ReceiveCommand(global.socket)) == NULL )
	exit(1);

    if( cmd->command < 0 || 
        (cmd->command > ClmMaxID && cmd->command < 1024) ||
        cmd->command > UserMaxID ||
        (cmd->command < 1024 && FunctionTable[cmd->command] == NULL ) ||
        (cmd->command >= 1024 && UserFunctionTable[cmd->command-1024] == NULL))
    {
        fprintf(stderr, "Bad Command ID = %d\n", cmd->command);
	abort();
    }
    if (cmd->command < 1024)
        rc = (*FunctionTable[cmd->command])(cmd);
    else
        rc = (*UserFunctionTable[cmd->command])(cmd);
    /*fprintf(stderr, "cmd = %x  cmd->command = %5d rc = %x\n", cmd, cmd->command, rc);*/

    if( rc != NULL ) {
	if( SendCommand(global.socket, rc) == -1 )
	    exit(1);
    }
    else {
	if( ! global.confirmed ) {
	    if( ConfirmCommand(global.socket) == -1 )
		exit(1);
	}
	global.confirmed = FALSE;
    }
    return(cmd);
}

void ClmCommandLoop()
{
    while( ! global.closed )
	ClmFreeCommand(ReceiveAndExecute());
}

ClmCommand *ClmCallbackCommandLoop()
{
    ClmCommand *cmd = NULL;

    if(global.terminated) {
	/* Should never occur at entry of this function, but sometimes does! */
	fprintf(stderr, "WARNING: Callback loop already terminated !\n");
	fflush(stderr);
    }
    
    while( ! global.terminated ) {
	cmd = ReceiveAndExecute();
	if( ! global.terminated )
	    ClmFreeCommand(cmd);
    }
    global.terminated = FALSE;
    return(cmd);
}

void DoOneCommand(client_data, source, id)
caddr_t    client_data;
int       *source;
XtInputId  id;
{
    ClmFreeCommand(ReceiveAndExecute());
}

XtInputId StartListeningToLisp()
{
    global.input_id = XtAppAddInput(global.app_context, global.socket, 
		                    XtInputReadMask, DoOneCommand, NULL);
}

void DiscontinueLisp()
{
    XtRemoveInput(global.input_id);
}

static ClmArg       version[1];
static ClmCommand   version_cmd = {ClmReturnValues, -1, 1, version};

main(argc, argv)
int    argc;
char **argv;
{
    int          rc, dummy_argc=0;
    char        *dummy;
    ClmCommand  *cmd;
    XEvent       event;

    
    /*malloc_debug(2);*/

    if( argc != 2 ) {
        fprintf(stderr, "usage: %s socket_number\n", argv[0] );
        exit(0);
    }

    /* Get the file descriptor of the socket stream */
    global.socket = atoi(argv[1]);

    /* Initialize global variables */
    global.app_shell_id = -1;
    global.app_shell_ptr = NULL;
    global.must_confirm_destroy = 0;
    global.main_loop_level = 0;

    /* Receive the application parameters */
    /* Receives four args: sync-mode, display-string, app-name and app-class */
    if( (cmd = ReceiveCommand(global.socket)) == NULL)
        abort();

    XtToolkitInitialize();
    global.app_context = XtCreateApplicationContext();
    
    XtAppSetErrorHandler(global.app_context, ClmErrorHandler);
    XtAppSetWarningHandler(global.app_context, ClmWarningHandler);

    /* Add the default action for the translation manager */
    global.OneAction[0].string = "clm";
    global.OneAction[0].proc   = ClmAction;
    XtAppAddActions(global.app_context, global.OneAction, 1);

    if( (global.display = XtOpenDisplay(global.app_context,
					StringArg0(cmd),
					StringArg1(cmd),
                                        StringArg2(cmd),
                                        NULL, 0, &dummy_argc, NULL)) == NULL) {
        SendError(global.socket,
		  global.error_message ? global.error_message :
		  global.warning_message ? global.warning_message :
		  "Cant't open display",
		  ClmEventError);
        exit(1);
    }


    /* Turn on synchronous mode if CLM_DEBUG is set. */
    if (getenv("CLM_DEBUG") != NULL) {
	XSynchronize(global.display, TRUE);
	fprintf(stderr, "toolkits running in synchronous mode.\n");
	fflush(stderr);
    }

    /* Confirm that XtOpenDisplay succeeded */
    /* Send back the current version number */
    version[0].arg_type = ClmArgInteger;
    version[0].v.int_value = XmVersion;
    if (SendCommand (global.socket, &version_cmd) == -1)
       abort();

    /*if( ConfirmCommand(global.socket) == -1 )
	abort();*/

    global.app_class = StringArg2(cmd);

    InitializeWidgetTable();
    InitializeClassTable();

    StartListeningToLisp();
    
    for(;;) {
	XtAppNextEvent(global.app_context, &event);
	XtDispatchEvent(&event);
	if( global.must_confirm_destroy ) {
	    global.must_confirm_destroy--;
	    NotifyDestroyed();
	}
    }
    /* NOTREACHED */
}




