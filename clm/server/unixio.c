static char sccsid[] = "@(#)unixio.c	1.7 9/15/92";

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

#include <sys/types.h>
#include <stdio.h>

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>

#include "interface.h"
#include "functions.h"
#include <stdlib.h>
#include "io.h"

static int ClmSerialNumber; /* Current serial number */
static int write_lisp_records = 0;
static int write_server_records = 0;
static int write_verbose = 0;

/* Send a command */

int SendCommand(sock, command )
int         sock;
ClmCommand *command;
{
    int i;

    command->serial = ClmSerialNumber;

    if( write_server_records )
	ClmPrintCommand(command);

    if(SendHeader(sock,command->command,command->serial,command->num_arg) == -1)
	return(-1);

    for(i=0; i<command->num_arg; ++i ) {
	switch( command->args[i].arg_type ) {
	    case ClmArgSymbol:
		if( SendSymbol(sock, command->args[i].v.symbol_value) == -1 )
	            return(-1);
		break;
	    case ClmArgInteger:
		if( SendInteger(sock, command->args[i].v.int_value) == -1 )
	            return(-1);
		break;
	    case ClmArgString:
		if( SendString(sock, command->args[i].v.string_value) == -1 )
	            return(-1);
		break;
	    case ClmArgFloat:
		if( SendFloat(sock, command->args[i].v.float_value) == -1 )
	            return(-1);
		break;
            default:
		fprintf(stderr, "ARG %d: illegal arg_type %d\n",
				 i, command->args[i].arg_type );
                abort();
	}
    }
    return(FlushBuffer(sock));
}

/* Receive an argument */

int ReceiveArgument(int sock, ClmArg* arg) {
    int   n;
    int   rc;
    char *in_rec_buf;
    char *ReceiveString();
    unsigned char buff[sizeof(arg->arg_type)];
    if(do_read(sock, buff, sizeof(arg->arg_type)) == -1 ) {
	return(-1);
    }
    memcpy(&(arg->arg_type), buff, sizeof(arg->arg_type));
    switch(arg->arg_type) {
	case ClmArgInteger:
	    arg->v.int_value = ReceiveInteger(sock,&rc);
	    return(rc);
	case ClmArgFloat:
	    arg->v.float_value = ReceiveFloat(sock,&rc);
	    return(rc);
	case ClmArgString:
	case ClmArgSymbol:
            in_rec_buf = ReceiveString(sock,&rc);
	    arg->v.string_value = ClmMalloc (strlen (in_rec_buf) + 1);
            strcpy (arg->v.string_value, in_rec_buf);
	    return(rc);
        default:
	    fprintf(stderr, "ReceiveArgument: illegal arg_type %d\n",
		    arg->arg_type);
	    return(-1);
    }
}

/* Receive a command */

ClmCommand *ReceiveCommand(int sock) {
  ClmCommand* cmd = (ClmCommand*)ClmMalloc(sizeof(ClmCommand));
  int i;
  unsigned char buff[sizeof(ClmCommand)];
  if (do_read(sock, buff, CommandSize) == -1) {
    return NULL;
  }
  memcpy(cmd, buff, sizeof(ClmCommand));
  /* Memory leak !!!!!! */
  /* Shouldn't allocated anything if num_arg == 0 */
  cmd->args =
    (ClmArg *)ClmMalloc((cmd->num_arg == 0 ? 1 : cmd->num_arg)*sizeof(ClmArg));

  for( i=0; i<cmd->num_arg; ++i )
    if( ReceiveArgument(sock, &(cmd->args[i])) == -1 )
      return(NULL);

  if( write_lisp_records )
    ClmPrintCommand(cmd);

  ClmSerialNumber = cmd->serial;

  return(cmd);
}

/* Confirm a command */
static ClmCommand Confirm = {ClmConfirm, -1, 0, NULL};

int ConfirmCommand(sock)
int sock;
{
    return( SendCommand(sock, &Confirm) );
}

/* Send a warning message */

ClmArg     ErrorArgs[2];
ClmCommand ErrorMessage = {ClmEvent, -1, 2, ErrorArgs};

/* Send an error or warning message */
void SendError(sock, message, opcode)
int   sock;
char *message;
int opcode;
{
    ErrorArgs[0].arg_type       = ClmArgInteger;
    ErrorArgs[0].v.int_value    = opcode;
    ErrorArgs[1].arg_type       = ClmArgString;
    ErrorArgs[1].v.string_value = message;
    if( SendCommand(sock, &ErrorMessage) == -1)
	abort();
    ClmFreeCommand(ClmCallbackCommandLoop());
}

/* Free the memory of a command structure allocated by ReceiveCommand() */

void ClmFreeCommand(cmd)
register ClmCommand *cmd;
{
    register int i;

    if( cmd ) {
	for(i=0; i<cmd->num_arg; ++i)
	    if( cmd->args[i].arg_type == ClmArgString ||
		cmd->args[i].arg_type == ClmArgSymbol )
		free(cmd->args[i].v.string_value);

	if(cmd->num_arg > 0)
	    free(cmd->args);

	free(cmd);
    }
}

void ClmPrintCommand(ClmCommand* command) {
    int i;

    if( command == NULL )
	return;

    fprintf(stderr, "command = %d   serial = %d num_arg = %d\n",
	    command->command, command->serial, command->num_arg);

    if( write_verbose ) {
	fprintf(stderr,"======================================\n");
	for(i=0; i<command->num_arg; i++) {
	    if(command->args[i].arg_type == ClmArgFloat)
		fprintf(stderr,"arg_type=ClmArgFloat  value=%f\n",
			command->args[i].v.float_value);
	    if(command->args[i].arg_type == ClmArgSymbol)
		fprintf(stderr,"arg_type=ClmArgSymbol  value=%s\n",
			command->args[i].v.symbol_value);
	    if(command->args[i].arg_type == ClmArgInteger)
		fprintf(stderr,"arg_type=ClmArgInteger  value=%d\n",
			command->args[i].v.int_value);
	    if(command->args[i].arg_type == ClmArgString)
		fprintf(stderr,"arg_type=ClmArgString   value=%s\n",
			command->args[i].v.string_value);
	}
	fprintf(stderr,"\n");
    }
    fflush(stderr);
}
