/* "@(#)io.h	1.6 1/28/92" */

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

/* Values for the ClmArg.arg_type and ClmArgHeader.arg_type fields */

#ifndef IO_H
#define IO_H

#define ClmArgInvalid (-1)
#define ClmArgInteger 0
#define ClmArgString  1
#define ClmArgSymbol  2
#define ClmArgFloat   3
/*#define ClmArgBinary  4*/

union ClmValue {
    int      int_value;
    char    *string_value;
    char    *symbol_value;
    float    float_value;
    /*caddr_t *binary_value;*/
};

typedef struct {
    int            arg_type;  /* Type of argument */
    union ClmValue v;
} ClmArg;

typedef struct {
    int     command;  /* Command to be executed */
    int     serial;   /* Serial number of request/callback */
    int     num_arg;  /* Number of arguments to be transferred */
    ClmArg *args;     /* List of arguments */
} ClmCommand;

typedef int WidgetID;

#define CommandSize (sizeof(ClmCommand)-sizeof(ClmArg *))

int FlushBuffer(int sock);
int SendFloat(int sock, float value);
int SendHeader(int sock, int code, int serial, int length);
int SendString(int sock, char* string);
int SendSymbol(int sock, char* symbol);
int SendSymbolL(int sock, char* symbol);
int ReceiveInteger(int sock, int* rc);
float ReceiveFloat(int sock, int* rc);
char* ReceiveString(int sock, int* rc);


#endif
