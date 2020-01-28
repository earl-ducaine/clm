static char sccsid[] = "@(#)runstatus.c	1.2 1/28/92";

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
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include "interface.h"
#include "functions.h"
#include <sys/types.h>
#include <sys/time.h>
#include <signal.h>
#include <unistd.h>

/* Code to implement the runstatus stuff */

/*
Lets hope that nothing is in the buffer when we want to do this.  Looking at the code which does not allocate
and has a mp::without-scheduling we should be OK
This code does assume that the buffer used for IO never overflows which does seem a little risky.
Lets malloc the buffer and realloc if it ever looks like overflowing.
*/

static int clmlockvalue = 0;

void exclclmlock(int x) {
    clmlockvalue += x;
}

int set_run_status (int socket, int widget, int status) {
  int packet[7];
  if (!clmlockvalue) {
    packet[0] = ClmRunStatus;
    packet[1] = 0;		/* serial */
    packet[2] = 2;		/* number of arguments */
    packet[3] = ClmArgInteger;
    packet[4] = widget;
    packet[5] = ClmArgInteger;
    packet[6] = status;
    return (write(socket,packet, sizeof(packet)));
  }
  else {
    return (0);
  }
}


// GC status


static int gc_widget = 0;
static int gc_fd = 0;

extern int (*gc_before)(),(*gc_after)();

int starting_gc() {
  if (gc_widget) {
    set_run_status(gc_fd, gc_widget, 2);
  }
  return 0;
}

int stopping_gc() {
  if (gc_widget) {
    set_run_status(gc_fd, gc_widget, 1);
  }
  return 0;
}

void init_gc_stuff(int fd, int x) {
    gc_widget = x;
    gc_fd = fd;
    gc_before = starting_gc;
    gc_after = stopping_gc;
    signal(SIGPIPE, SIG_IGN);
}
