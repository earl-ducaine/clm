static char sccsid[] = "@(#)exclio.c	1.6 1/28/92";

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
 * Authors: Thomas Spandoeck
 *          VW-GEDAS
 *          Pascalstr. 11
 *          D-1000 Berlin 10
 *
 *          Andreas Baecker (baecker@gmdzi.gmd.de)
 *          P.O. Box 1316
 *          D-5205 Sankt Augustin 1
 */

#include <sys/types.h>
#include <stdio.h>
#include "io.h"
#include <lisp.h>  /* Allegro supplied file */


/*  Read a number of bytes from the socket.
 *  Try again on partial reads.
 *  Return -1 on failure
 */

extern int do_read();

#define READ_BYTES(n_bytes, buf) if( do_read(sock, buf, n_bytes) == -1) \
				     { *rc = -1; return(0); } else *rc = 0

char *ReceiveEXCLString(sock, index, length, rc)
int  sock;
int *rc;
{
    char *string_area;

    string_area = (char *) Vecdata(SymbolValue(lisp_value(index)));
    READ_BYTES(length, string_area);
    /* *(string_area+length) = '\0'; */
    return((char *) SymbolValue(lisp_value(index)));
}
