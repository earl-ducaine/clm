static char sccsid[] = "@(#)Text2.c	1.3 1/28/92";

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
 *
 * This file contributed by:
 *          Peter.Cousseau@A.NL.CS.CMU.EDU
 */

#include <ctype.h>
#include <stdio.h>
#include <math.h>
#include <strings.h>

#include <Xm/Xm.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <Xm/Text.h>
#if XmREVISION != 0
#include <Xm/TextF.h>
#endif

#include "interface.h"
#include "functions.h"

ClmCommand *ClmFtextTester(cmd)
ClmCommand *cmd;
{

  Widget widget;
  char *msg;
  XEvent *event;
  String *arg_string;
  

  if (msg = LookupWidget(IntArg0(cmd), &widget, NULL, NULL))
    GenWarn(msg);

  arg_string =  (String *)malloc( sizeof(String));
  *arg_string = StringArg2(cmd);

  printf (" %s\n", *arg_string);

#if XmREVISION != 0
  XtCallActionProc(widget, StringArg1(cmd), event, arg_string, IntArg3(cmd));
#endif

  if (global.forced_output)
    XmUpdateDisplay(widget); 

  return(NULL);
}



ClmCommand *ClmFtextSetHighlight(cmd)
ClmCommand *cmd;
{
  Widget widget;
  char *msg;
  XmTextPosition start;
  XmTextPosition end;
#if XmREVISION != 0
  XmHighlightMode mode;
#endif
  int mode_number;

  if (cmd->command != ClmTextSetHighlight || cmd->num_arg != 4)
    GenError("text-set-highlight: illegal command record");

  if( msg = LookupWidget(IntArg0(cmd), &widget, NULL, NULL))
    GenWarn(msg);

  start = IntArg1(cmd);
  end = IntArg2(cmd);
  mode_number = IntArg3(cmd);

#if XmREVISION != 0
  if (mode_number == 0)
    mode = XmHIGHLIGHT_NORMAL;
  else if (mode_number == 1)
    mode = XmHIGHLIGHT_SELECTED;
  else if (mode_number == 2)
    mode = XmHIGHLIGHT_SECONDARY_SELECTED;
  else
#endif
    GenWarn("Error in selection mode.");

#if XmREVISION != 0
  if( XtClass(widget) == (WidgetClass)xmTextWidgetClass )
    XmTextSetHighlight (widget, start, end, mode);
  if( XtClass(widget) == (WidgetClass)xmTextFieldWidgetClass )
    XmTextFieldSetHighlight (widget, start, end, mode);

  if (global.forced_output)
    XmUpdateDisplay(widget);

  return(NULL);
#endif
}


static ClmArg last_pos_args[1];
static ClmCommand last_pos_cmd = { ClmReturnValues, -1, 1, last_pos_args };

ClmCommand *ClmFtextGetLastPosition(cmd)
ClmCommand *cmd;
{
  Widget widget;
  char *msg;
 
  if (cmd->command != ClmTextGetLastPosition || cmd->num_arg != 1)
    GenError("text-get-last-position: illegal command record");

  if ( msg = LookupWidget(IntArg0(cmd), &widget, NULL, NULL))
    GenWarn(msg);

  last_pos_cmd.args[0].arg_type = ClmArgInteger;
  if( XtClass(widget) == (WidgetClass)xmTextWidgetClass )
    last_pos_cmd.args[0].v.int_value = XmTextGetLastPosition(widget);
#if XmREVISION != 0
  if( XtClass(widget) == (WidgetClass)xmTextFieldWidgetClass )
    last_pos_cmd.args[0].v.int_value = XmTextFieldGetLastPosition(widget);
#endif

  return(&last_pos_cmd);
}


ClmCommand *ClmFtextSetSelection(cmd)
ClmCommand *cmd;
{
  Widget widget;
  XmTextPosition first, last;
  Time time;
  char *msg;

  if (cmd->command != ClmTextSetSelection || cmd->num_arg != 3)
    GenError("text-set-selection: illegal command record");

  if (msg = LookupWidget(IntArg0(cmd), &widget, NULL, NULL))
    GenWarn(msg);

  first = IntArg1(cmd);
  last = IntArg2(cmd);
#if XmREVISION != 0
  time = XtLastTimestampProcessed (global.display);
#else
  time = CurrentTime;
#endif

  if( XtClass(widget) == (WidgetClass)xmTextWidgetClass )
    XmTextSetSelection(widget, first, last, time);
#if XmREVISION != 0
  if( XtClass(widget) == (WidgetClass)xmTextFieldWidgetClass )
    XmTextSetSelection(widget, first, last, time);
#endif

  if( global.forced_output )
    XmUpdateDisplay(widget);

  return(NULL);
}


static ClmArg insert_pos_args[1];
static ClmCommand insert_pos_cmd = { ClmReturnValues, -1, 1, insert_pos_args };

ClmCommand *ClmFtextGetInsertionPosition(cmd)
ClmCommand *cmd;
{
  Widget widget;
  char *msg;

  if (cmd->command != ClmTextGetInsertionPosition || cmd->num_arg != 1)
    GenError("text-get-insertion-position: illegal command record");

  if (msg = LookupWidget(IntArg0(cmd), &widget, NULL, NULL))
    GenWarn(msg);

  insert_pos_cmd.args[0].arg_type = ClmArgInteger;
  if( XtClass(widget) == (WidgetClass)xmTextWidgetClass )
    insert_pos_cmd.args[0].v.int_value = XmTextGetInsertionPosition(widget);
#if XmREVISION != 0
  if( XtClass(widget) == (WidgetClass)xmTextFieldWidgetClass )
    insert_pos_cmd.args[0].v.int_value = 
                                 XmTextFieldGetInsertionPosition(widget);
#endif

  return(&insert_pos_cmd);
}



ClmCommand *ClmFtextReplace(cmd)
ClmCommand *cmd;
{
  Widget widget; 
  XmTextPosition start_pos;
  XmTextPosition end_pos;
  char *msg, *new_value;
/*  XmSTRING_DEFAULT_CHARSET str; */

  if (cmd->command != ClmTextReplace || cmd->num_arg != 4)
    GenError("text-replace: illegal command record");

  if( msg = LookupWidget(IntArg0(cmd), &widget, NULL, NULL) )
    GenWarn(msg);  

  start_pos = IntArg1(cmd);
  end_pos = IntArg2(cmd);

/*  str = XmStringCreateLtoR(StringArg3(cmd), XmSTRING_DEFAULT_CHARSET); */

  new_value = StringArg3(cmd); 

  if( XtClass(widget) == (WidgetClass)xmTextWidgetClass )
    XmTextReplace(widget, start_pos, end_pos, new_value);
#if XmREVISION != 0
  if( XtClass(widget) == (WidgetClass)xmTextFieldWidgetClass )
    XmTextFieldReplace(widget, start_pos, end_pos, new_value);
#endif

  if( global.forced_output )
    XmUpdateDisplay(widget);

  return(NULL);
}

/* ******************************************************** */

static ClmArg substring_args[1];
static ClmCommand substring_cmd = { ClmReturnValues, -1, 1, substring_args };

ClmCommand *ClmFtextGetSubstring(cmd)
ClmCommand *cmd;
{
  Widget widget;
  char *the_string, *substring, *msg;
  int substring_length, start_pos;

  if (cmd->command != ClmTextGetSubstring || cmd->num_arg != 3)
    GenError("text-get-substring: illegal command record");

  if( msg = LookupWidget(IntArg0(cmd), &widget, NULL, NULL))
    GenWarn(msg);

  if( XtClass(widget) == (WidgetClass)xmTextWidgetClass )
    the_string = XmTextGetString(widget); 
#if XmREVISION != 0
  if( XtClass(widget) == (WidgetClass)xmTextFieldWidgetClass )
    the_string = XmTextFieldGetString(widget);
#endif
  start_pos = IntArg1(cmd); 
  substring_length = IntArg2(cmd); 

  if (start_pos == -1) 
    start_pos = (int)XmTextGetInsertionPosition(widget);

  if (substring_length < 0) {
    start_pos = start_pos + substring_length;
    substring_length = -substring_length;
  }

  substring = (char *)malloc( (substring_length + 1) * sizeof(char));
  strncpy(substring, (the_string + start_pos), substring_length);
  substring[substring_length] = 0;

  XtFree(the_string);

  substring_cmd.args[0].arg_type = ClmArgString;
  substring_cmd.args[0].v.string_value = substring;
  return(&substring_cmd);
}


/* *********************************************************** */

static ClmArg search_args[1];
static ClmCommand search_cmd = { ClmReturnValues, -1, 1, search_args };

ClmCommand *ClmFtextSearch(cmd)
ClmCommand *cmd;
{
  Widget  widget;
  char    *msg, *the_string, *text_ptr;
  XmTextPosition start_pos, end_pos, last_pos, ins_pos;
  int string_length, search_ret, case_sen, backwardp;

  if(cmd->command != ClmTextSearch || cmd->num_arg != 6)
    GenError("text-search: illegal command record");

  if( msg = LookupWidget(IntArg0(cmd), &widget, NULL, NULL))
    GenWarn(msg);

  if( XtClass(widget) == (WidgetClass)xmTextWidgetClass ) {
    text_ptr = XmTextGetString(widget);
    last_pos = XmTextGetLastPosition(widget);
    ins_pos = XmTextGetInsertionPosition(widget);
  }
#if XmREVISION != 0
  if( XtClass(widget) == (WidgetClass)xmTextFieldWidgetClass ) {
    text_ptr = XmTextFieldGetString(widget);
    last_pos = XmTextFieldGetLastPosition(widget);
    ins_pos = XmTextFieldGetInsertionPosition(widget);
  }
#endif
  the_string = StringArg1(cmd);
  string_length = strlen(the_string);
  start_pos = IntArg2(cmd);
  end_pos = IntArg3(cmd);
  case_sen = IntArgn(cmd, 4);
  backwardp = IntArgn(cmd, 5);

  if (backwardp) {      /* we are searching backward */
    if (end_pos == -2) {
      end_pos = (last_pos - 1);
    }
    if (end_pos == -1) {
      end_pos = (ins_pos - 1);
    }
    if (start_pos == -1) {
      start_pos = 0;
    }
  }  
  else {     /* we are searching forward */
    if (end_pos == -1) {
      end_pos = (last_pos - 1);
    }
    if (start_pos == -1) {
      start_pos = ins_pos;
    }
  }

  if (backwardp) {
    search_ret =
      Simple_R_Search(text_ptr, the_string, string_length, start_pos, end_pos, case_sen);
  }
  else {
    search_ret = 
      Simple_Search(text_ptr, the_string, string_length, start_pos, end_pos, case_sen); 
  }

/*  printf (" start_pos=%ld\n end_pos=%ld\n search_ret=%ld\n", start_pos, end_pos, search_ret);  */

  if ( ((search_ret + string_length) > (end_pos + 1)) || (search_ret < start_pos) ) {
    search_cmd.args[0].arg_type = ClmArgSymbol;
    search_cmd.args[0].v.symbol_value = "NIL";
  }
  else {
    search_cmd.args[0].arg_type = ClmArgInteger;
    search_cmd.args[0].v.int_value = search_ret;
  }  /* end of if */

  XtFree(text_ptr);

  return(&search_cmd);
}



/*  The search code is modified from the Rabin-Karp Algorithm in */
/*     "Algorithms" by Robert Sedgewick.                         */
/*                                                               */
/*    M is the number of chars in the search string.             */

int Simple_Search(text, string, M, start_pos, end_pos, case_sen)
int M, start_pos, end_pos;
char *text, *string;
Boolean case_sen;
{
  register int i;
  int d;
  long int q, h1, h2, dM;

  d = 32;
  q = 33554393;
  end_pos = end_pos + 1;
  dM = 1;
  for (i=1; i<=(M - 1); i++) {
    dM = (d*dM)%q;
  }
  h1 = 0;
  h2 = 0;
  if (case_sen) 
    {
      for (i=0; i<M; i++) {
	h1 = (h1*d+toascii(string[i]))%q;
      }
      for (i = start_pos; i < (start_pos + M); i++) {
	h2 = (h2*d+toascii(text[i]))%q;
      }
      i = start_pos;
      while ( (h1 != h2) && (i <= (end_pos - M)) ) {
	h2 = (h2+d*q-toascii(text[i])*dM)%q;
	h2 = (h2*d+toascii(text[i+M]))%q;
	i++;
      }
    }    /* end of if 1 */
  else
    {
      for (i=0; i<M; i++) {
	h1 = (h1*d+to_lowercase_ascii(string[i]))%q;
      }
      for (i = start_pos; i < (start_pos + M); i++) {
	h2 = (h2*d+to_lowercase_ascii(text[i]))%q;
      }
      i = start_pos;
      while ( (h1 != h2) && (i <= (end_pos - M)) ) {
	h2 = (h2+d*q-to_lowercase_ascii(text[i])*dM)%q;
	h2 = (h2*d+to_lowercase_ascii(text[i+M]))%q;
	i++;
      }
    } /* end of if */

  return(i);
}


int to_lowercase_ascii (the_char)
char the_char;
{
  if (isupper(the_char)) {
    return( (toascii(the_char) + 32));
    }
  else {
    return( toascii(the_char) );
  }
}

/*******************************************************************/
/*   Reverse Search                                                */
/*******************************************************************/



int Simple_R_Search(text, string, M, start_pos, end_pos, case_sen)
int M, start_pos, end_pos;
char *text, *string;
Boolean case_sen;
{
  register int i;
  int d;
  long int q, h1, h2, dM;

  d = 32;
  q = 33554393;
  dM = 1;
  for (i=1; i<=(M - 1); i++) {
    dM = (d*dM)%q;
  }
  h1 = 0;
  h2 = 0;
  if (case_sen) 
    {
      for (i=(M-1); i>=0; i--) {
	h1 = (h1*d+toascii(string[i]))%q;
      }
      for (i=end_pos; i > (end_pos - M); i--) {
	h2 = (h2*d+toascii(text[i]))%q;
      }
      i = end_pos;
      while ( (h1 != h2) && (i >= (start_pos + M - 1)) ) {
	h2 = (h2+d*q-toascii(text[i])*dM)%q;
	h2 = (h2*d+toascii(text[i-M]))%q;
	i--;
      }
    }    /* end of if 1 */
  else
    {
      for (i=(M-1); i>=0; i--) {
	h1 = (h1*d+to_lowercase_ascii(string[i]))%q;
      }
      for (i = end_pos; i > (end_pos - M); i--) {
	h2 = (h2*d+to_lowercase_ascii(text[i]))%q;
      }
      i = end_pos;
      while ( (h1 != h2) && (i >= (start_pos + M - 1)) ) {
	h2 = (h2+d*q-to_lowercase_ascii(text[i])*dM)%q;
	h2 = (h2*d+to_lowercase_ascii(text[i-M]))%q;
	i--;
      }
    } /* end of if */

  return( (i - M + 1) );
}
