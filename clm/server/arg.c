static char sccsid[] = "@(#)arg.c	1.6 1/28/92";

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

#include <Xm/Xm.h>
#include "interface.h"
#include "functions.h"

#ifdef ARG_DEBUG

#ifdef HPUX  /* The HP preprocessor has difficulties with \n in macros */
#define CheckArgInRange(cmd, n) { if( cmd->num_arg < 0 || n >= cmd->num_arg ){\
	fprintf(stderr, "Argument number <%d> out of range!", n); \
	fprintf(stderr, "cmd = %d, num_arg = %d\n",cmd->command,cmd->num_arg);}}
#else
#define CheckArgInRange(cmd, n) { if( cmd->num_arg < 0 || n >= cmd->num_arg ){\
	fprintf(stderr, "Argument number <%d> out of range!\n", n); \
	fprintf(stderr, "cmd = %d, num_arg = %d\n",cmd->command,cmd->num_arg);}}
#endif

#define CheckIntArg(cmd, n)    { CheckArgInRange(cmd, n); \
				 if(cmd->args[n].arg_type != ClmArgInteger) { \
			             arg_error(cmd,"Integer",n); \
				     abort(); } \
			         else \
			             return(cmd->args[n].v.int_value); }
#define CheckStringArg(cmd, n) { CheckArgInRange(cmd, n); \
				 if(cmd->args[n].arg_type != ClmArgString) { \
			             arg_error(cmd,"String",n); \
				     abort(); } \
			         else \
			             return(cmd->args[n].v.string_value); }
#define CheckSymbolArg(cmd, n) { CheckArgInRange(cmd, n); \
				 if(cmd->args[n].arg_type != ClmArgSymbol) { \
			             arg_error(cmd,"Symbol",n); \
				     abort(); } \
			         else \
			             return(cmd->args[n].v.symbol_value); }
#else
#define CheckIntArg(cmd, n)    { return(cmd->args[n].v.int_value); }
#define CheckStringArg(cmd, n) { return(cmd->args[n].v.string_value); }
#define CheckSymbolArg(cmd, n) { return(cmd->args[n].v.symbol_value); }
#endif

void arg_error(cmd, expected, arg_no)
ClmCommand *cmd;
char *expected;
int arg_no;
{
    fprintf(stderr,
	    "Illegal argument type for argument at position %d.\n", arg_no);
    fprintf(stderr, "Expected type: %s.\n", expected);
    fflush(stderr);
    ClmPrintCommand(cmd);
}

int IntArg0(cmd)
ClmCommand *cmd;
{
    CheckIntArg(cmd, 0);
}

int IntArg1(cmd)
ClmCommand *cmd;
{
    CheckIntArg(cmd, 1);
}

int IntArg2(cmd)
ClmCommand *cmd;
{
    CheckIntArg(cmd, 2);
}

int IntArg3(cmd)
ClmCommand *cmd;
{
    CheckIntArg(cmd, 3);
}

int IntArgn(cmd,n)
ClmCommand *cmd;
int n;
{
    CheckIntArg(cmd, n);
}

char *StringArg0(cmd)
ClmCommand *cmd;
{
    CheckStringArg(cmd, 0);
}

char *StringArg1(cmd)
ClmCommand *cmd;
{
    CheckStringArg(cmd, 1);
}

char *StringArg2(cmd)
ClmCommand *cmd;
{
    CheckStringArg(cmd, 2);
}

char *StringArg3(cmd)
ClmCommand *cmd;
{
    CheckStringArg(cmd, 3);
}

char *StringArgn(cmd,n)
ClmCommand *cmd;
int n;
{
    CheckStringArg(cmd, n);
}

char *SymbolArg0(cmd)
ClmCommand *cmd;
{
    CheckSymbolArg(cmd, 0);
}

char *SymbolArg1(cmd)
ClmCommand *cmd;
{
    CheckSymbolArg(cmd, 1);
}

char *SymbolArg2(cmd)
ClmCommand *cmd;
{
    CheckSymbolArg(cmd, 2);
}

char *SymbolArg3(cmd)
ClmCommand *cmd;
{
    CheckSymbolArg(cmd, 3);
}

char *SymbolArgn(cmd,n)
ClmCommand *cmd;
int n;
{
    CheckSymbolArg(cmd, n);
}
