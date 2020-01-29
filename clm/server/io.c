static char sccsid[] = "@(#)io.c	1.7 7/20/92";

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
#include <errno.h>
#include <stdio.h>
#include "io.h"
#include <ctype.h>
#include <strings.h>
#include <string.h>
#include <unistd.h>
#include <stdlib.h>

void* realloc(void*, long unsigned int);
void* malloc(long unsigned int);

int FlushBuffer(int sock);

#define SERVER_BUFSIZ BUFSIZ

struct {
    int pos;
    char buf[SERVER_BUFSIZ];
} output_buffer;

/*  Append a number of bytes to the output buffer.
 *  Flush output buffer if an overflow is detected.
 */

int send_bytes(ptr, size, sock)
char *ptr;
int size;
int sock;
{
    int to_send = size;
    int n_bytes;

    while(to_send > 0) {
	n_bytes = to_send > SERVER_BUFSIZ - output_buffer.pos ?
		  SERVER_BUFSIZ - output_buffer.pos : to_send;
	if( n_bytes > 0)
	    bcopy(ptr + size-to_send, output_buffer.buf + output_buffer.pos,
		  n_bytes);
        output_buffer.pos += n_bytes;
	if(output_buffer.pos == SERVER_BUFSIZ) {
	    if( FlushBuffer(sock) == -1)
		return -1;
	}
        to_send -= n_bytes;
    }
    return(0);
}

#define SEND_BYTES(ptr, size) { if( send_bytes(ptr, size, sock) == -1) \
				return(-1); }

/* Send a String */

static int ArgIsString  = ClmArgString;
static int ArgIsInteger = ClmArgInteger;
static int ArgIsSymbol  = ClmArgSymbol;
static int ArgIsFloat   = ClmArgFloat;

int SendInteger(sock, value )
int sock;
int value;
{
    SEND_BYTES(&ArgIsInteger,sizeof(int));
    SEND_BYTES(&value,sizeof(int));
    return(0);
}

int SendFloat(int sock, float value) {
    SEND_BYTES(&ArgIsFloat,sizeof(int));
    SEND_BYTES(&value,sizeof(float));
    return(0);
}

/* uppercasing version */

/* maintain a temporary string buffer */

char *get_temp_string_buffer(int len) {
    static char *temp;
    static int length = 0;

    if (length == 0 || (length < len)) {
	length = len > 4096 ? len : 4096;
	if (temp == NULL)
	    temp = malloc(length);
	else
	    temp = realloc(temp, length);
    }
    return(temp);
}

char* strupcase(string, length)
char* string;
int length;
{
    char* otemp = get_temp_string_buffer(length);
    char *temp = otemp;
    char *ostring = string;
    int i = 0;
    for (; i < length ; i++) {
	char c = *(string++);
	*(temp++) = islower(c) ? toupper(c) : c;
    }
    *temp = *string; /* terminator */
    /*fprintf(stderr, "strupcase got %s, %s\n", ostring, otemp);*/
    return(otemp);
}


char* strdowncase(string, length)
char* string;
int length;
{
    char* otemp = get_temp_string_buffer(length);
    char *temp = otemp;
    char *ostring = string;
    int i = 0;
    for (; i < length ; i++) {
	char c = *(string++);
	*(temp++) = isupper(c) ? tolower(c) : c;
    }
    *temp = *string; /* terminator */
    /*fprintf(stderr, "strdowncase got %s, %s\n", ostring, otemp);*/
    return(otemp);
}


int SendSymbolL(int sock, char* symbol) {
    int length = (symbol ? (*symbol ? strlen(symbol) : 0) : 0);
    char  *string = strupcase(symbol,length);

    SEND_BYTES(&ArgIsSymbol,sizeof(int));
    SEND_BYTES(&length,sizeof(int));
    if( length > 0)
	SEND_BYTES(string ,length);
    return(0);
}

int SendSymbol(int sock, char* symbol) {
    int length = (symbol ? (*symbol ? strlen(symbol) : 0) : 0);

    SEND_BYTES(&ArgIsSymbol,sizeof(int));
    SEND_BYTES(&length,sizeof(int));
    if( length > 0)
	SEND_BYTES(symbol,length);
    return(0);
}

int SendString(int sock, char* string) {
    int length = (string ? (*string ? strlen(string) : 0) : 0);

    SEND_BYTES(&ArgIsString,sizeof(int));
    SEND_BYTES(&length,sizeof(int));
    if( length > 0)
	SEND_BYTES(string,length);
    return(0);
}

int SendHeader(int sock, int code, int serial, int length) {
    /* Force reset of buffer (it may not be reset because of a previous crash)*/
    output_buffer.pos = 0;
    SEND_BYTES(&code,sizeof(int));
    SEND_BYTES(&serial,sizeof(int));
    SEND_BYTES(&length,sizeof(int));
    return(0);
}

int FlushBuffer(int sock) {
    int n;
    int not_written;

    errno = 0;
    not_written = output_buffer.pos;
    while( (n = write(sock,output_buffer.buf+output_buffer.pos-not_written,
		      not_written)) != not_written) {
	if( n > 0 ) {
	    not_written -= n;
	}
	else {
/*	    perror("write");*/
	    fflush(stderr);
            output_buffer.pos = 0;
	    return(-1);
	}
    }
    output_buffer.pos = 0;
    return(0);
}

/*  Read a number of bytes from the socket.
 *  Try again on partial reads.
 *  Return -1 on failure
 */

int do_read(int sock, char* ptr, int size) {
    int not_read, n, n_intr = 0;

    not_read = size;
    errno  = 0;
    while ( (n = read(sock, ptr + size-not_read, not_read)) != not_read) {
	if( n > 0 ) {
	    not_read -= n;
	}
	else {
	    if( errno == EINTR && n_intr < 10) {
		n_intr++;
		/* fprintf(stderr, "Interupt %d in \n", n_intr); */
		errno = 0;
	    }
	    else {
/*		perror("read");*/
		return(-1);
	    }
	}
    }
    return(0);
}

#define READ_BYTES(n_bytes, buf) \
  if(do_read(sock, (buf), (n_bytes)) == -1) {	\
    *rc = -1; \
    return 0;					\
  } else { \
    *rc = 0; \
  }


/* int bytes_to_integer (unsigned char* bytes) { */
/*   unsigned char bytes[4]; */
/*   unsigned long n = 175; */

/*   bytes[0] = (n >> 24) & 0xFF; */
/*   bytes[1] = (n >> 16) & 0xFF; */
/*   bytes[2] = (n >> 8) & 0xFF; */
/*   bytes[3] = n & 0xFF; */
/* } */

// bytes should be array of bytes of size(int).
void integer_to_bytes (int integer, unsigned char* bytes) {
  int number_bytes = sizeof(int);
  int bits_to_shift;
  for (int i = 0; i < number_bytes; i--) {
    bytes[i] = (integer >> (i * 8)) & 0xFF;
  }
}

// bytes should be array of bytes of size(int).
int bytes_to_integer (unsigned char* bytes) {
  int number_bytes = sizeof(int);
  int bits_to_shift;
  int integer = 0;
  for (int i = number_bytes - 1; i == 0; i--) {
    integer = (integer << 8) | bytes[i];
  }
  return integer;
}

// bytes should be array of bytes of size(int).
float bytes_to_float (unsigned char* bytes) {
  fprintf (stderr, "Error: bytes_to_float not implemented");
  return 0;
}

int receiveinteger(int sock, int* rc) {
  int buffer_size = sizeof(int);
  char value[buffer_size];
  int n;
  READ_BYTES(sizeof(int), (value));
  *rc = 0;
  return bytes_to_integer(value);
}

// Clearly not right
float ReceiveFloat(int sock, int* rc) {
  int buffer_size = sizeof(float);
  char value[buffer_size];
  *rc = 0;
  READ_BYTES(sizeof(float), (value));
  return bytes_to_float(value);
}

char *readstrb = NULL;
  int readblength = -1;

char* ReceiveString(int sock, int* rc) {
  int buffer_size = 256;
  char value[buffer_size];
  int n;
  READ_BYTES(buffer_size, value);
  if (buffer_size >= readblength) {
    if (readblength != -1) {
      free (readstrb);
    }
    readblength = buffer_size + 50;
    readstrb = malloc(readblength);
    if(readstrb == NULL) {
      perror("malloc");
      *rc = -1;
      return NULL;
    }
  }
  // Is this a retry?
  READ_BYTES(length, readstrb);
  *(readstrb + buffer_size) = '\0';
  return readstrb;
}
