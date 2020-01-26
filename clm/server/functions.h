/* "@(#)functions.h	1.7 1/30/92" */

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

/* Forward declarations */

/* Functions from main.c */

void ClmCommandLoop(/**/);
char *ClmMalloc(/* char * */);
void NotifyDestroyed(), DoOneCommand();
ClmCommand *ClmCallbackCommandLoop(/**/);
   
/* Functions from protocol.c */

int SendInteger(/* int pipe; int value */);
int SendString(/* int pipe; char *string */);
int SendCommand(/* int pipe; ClmCommand *command */);
int ReceiveArgument(/* int ppe; ClmArg *arg */);
ClmCommand *ReceiveCommand(/* ind pipe */);
int ConfirmCommand(/* int pipe */);
void SendError(/* int pipe; char *message; int opcode */ );
ClmCommand *CheckConfirm(/* int pipe */);
void ClmFreeCommand(/* ClmCommand *cmd */);
void ClmPrintCommand(/* ClmCommand */);

/* Functions from arg.c */
int IntArg0(); int IntArg1(); int IntArg2(); int IntArg3(); int IntArgn();
char *StringArg0(); char *StringArg1(); char *StringArg2(); char *StringArg3();
char *StringArgn();
char *SymbolArg0(); char *SymbolArg1(); char *SymbolArg2(); char *SymbolArg3();
char *SymbolArgn();
void arg_error();

/* Functions from ActionMgr.c */
extern void  ClmAction();

/* Functions from Classes.c */
int ClassNameToIndex(/* char *name */);
int ClassPointerToIndex(/* WidgetClass class */);

/* Functions from Widgets.c */

WidgetID GenWidgetID();
void  InitializeWidgetTable();
char *InsertWidget(/* id, widget, class, parent_class */);
char *LookupWidget(/* id, widget_ptr, class_id_ptr, node_ptr*/);
char *LookupClassOfParentOfWidget(/* id, parent_class_ptr */);
void RegisterDestroyCallback( /* Widget WidgetID */);
WidgetID FindWidget(/* Widget */);

/* Functions from WidgetMgr.c */

#define EW_GENERATE_NEW_ID (-10)
#define EW_GENERATE_ID_IF_NECESSARY (-11)
#define EW_COMPUTE_CLASS_ID (-10)
WidgetID EncapsulateWidget ();

/* Functions from Resources.c */

caddr_t ClmCvtToToolkit(/* char *resource_name; caddr_t value;char **msg_ptr*/);
void    ClmCvtToExternal(/* char *resource_name; Widget widget; int *type;
                          caddr_t *value; char **msg_ptr */ );
/* Functions from Callbacks.c */

char *ClmCallbackAdd(/* Widget widget; WidgetID id; char *list */ );
char *ClmCallbackRemove(/* Widget widget; WidgetID id; char *list */ );

/* Functions from Events.c */

char *ClmEventAdd(/* widget; widgetid; event_mask; nonmaskable */);
char *ClmEventRemove(/* widget; widgetid; event_mask; nonmaskable */);

/* Functions from Protocol.c */

char *ClmProtocolCallbackAdd(/* Widget widget; WidgetID id; char *prop, prot*/);
char *ClmProtocolCallbackRemove(/*Widget widget;WidgetID id;char *prop,prot*/);

/* Functions from ResourceMgr.c */

Arg *ClmMakeArgList();

/* Functions from Warnings.c */

ClmCommand *do_warn();
ClmCommand *do_error();
