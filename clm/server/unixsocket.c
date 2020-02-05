static char sccsid[] = "@(#)unixsocket.c	1.10 12/20/93";

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

/* Copyright    Massachusetts Institute of Technology    1988	*/
/*
 * THIS IS AN OS DEPENDENT FILE! It should work on 4.2BSD derived
 * systems.  VMS and System V should plan to have their own version.
 *
 * This code was cribbed from lib/X/XConnDis.c.
 * Compile using
 *                    % cc -c socket.c
 */

/* Adapted by GINA */
/* CHANGES:   - socket path to /tmp/clm_socket */
/*            - changed second parameter to xt_tcp_port */

#include <stdio.h>
#include <X11/Xos.h>
#include <X11/Xproto.h>
#include <errno.h>
#include <netinet/in.h>
#include <sys/ioctl.h>
#include <netdb.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/wait.h>
#include <arpa/inet.h>

#ifndef hpux
#include <netinet/tcp.h>
#endif

#include <fcntl.h>
#include <signal.h>

#include <sys/un.h>
#ifndef SOCKET_PATH
#define SOCKET_PATH "/tmp/clm_socket"
#endif

void bcopy();

/*
 * Attempts to connect to server, given host and xt_tcp_port. Returns file
 * descriptor (network socket) or 0 if connection fails.
 */

int connect_to_toolkit_server (host, xt_tcp_port)
     char *host;
     int xt_tcp_port;
{
  struct sockaddr_in inaddr;	/* INET socket address. */
  struct sockaddr *addr;		/* address to connect to */
  struct hostent *host_ptr;
  int addrlen;			/* length of address */
  struct sockaddr_un unaddr;	/* UNIX socket address. */
  extern char *getenv();
  extern struct hostent *gethostbyname();
  int fd;				/* Network socket */
  {
    if ((host[0] == '\0') ||
	(strcmp("unix", host) == 0)) {
	/* Connect locally using Unix domain. */
	unaddr.sun_family = AF_UNIX;
	(void) strcpy(unaddr.sun_path, SOCKET_PATH);
	addr = (struct sockaddr *) &unaddr;
	addrlen = strlen(unaddr.sun_path) + 2;
	/*
	 * Open the network connection.
	 */
	if ((fd = socket((int) addr->sa_family, SOCK_STREAM, 0)) < 0)
	    return(-1);	    /* errno set by system call. */
    } else {
      /* Get the statistics on the specified host. */
      if ((inaddr.sin_addr.s_addr = inet_addr(host)) == -1)
	{
	  if ((host_ptr = gethostbyname(host)) == NULL)
	    {
	      /* No such host! */
	      errno = EINVAL;
	      return(-1);
	    }
	  /* Check the address type for an internet host. */
	  if (host_ptr->h_addrtype != AF_INET)
	    {
	      /* Not an Internet host! */
	      errno = EPROTOTYPE;
	      return(-1);
	    }
	  /* Set up the socket data. */
	  inaddr.sin_family = host_ptr->h_addrtype;

/* Fatal compiler bug reported by Gene Libardi */
#ifdef hpux
	  bcopy((char *)&host_ptr->h_addr_list[0],
#else
	  bcopy((char *)host_ptr->h_addr,
#endif
		(char *)&inaddr.sin_addr,
		sizeof(inaddr.sin_addr));
	}
      else
	{
	  inaddr.sin_family = AF_INET;
	}
      addr = (struct sockaddr *) &inaddr;
      addrlen = sizeof (struct sockaddr_in);
      inaddr.sin_port = xt_tcp_port;
      inaddr.sin_port = htons(inaddr.sin_port);
      /*
       * Open the network connection.
       */
      if ((fd = socket((int) addr->sa_family, SOCK_STREAM, 0)) < 0){
	  return(-1);	    /* errno set by system call. */}

          /* make sure to turn off TCP coalescence */
          /*{ int mi = 1;
	    setsockopt (fd, IPPROTO_TCP, TCP_NODELAY, &mi, sizeof (int));
          }*/
    }
    if (connect(fd, addr, addrlen) == -1)
      {
	(void) close (fd);
	return(-1); 	    /* errno set by system call. */
      }
  }

  /*
   * Return the id if the connection succeeded.
   */

  return(fd);
}

int listen_to_socket(fd)
int fd;
{
    long nbytes;

    if( ioctl(fd,FIONREAD,&nbytes) == -1 )
	return(-1);
    return( nbytes ? 1 : 0 );
}

int CloseStream(stream)
int stream;
{
    return(close(stream));
}

int is_directory(path)
char *path;
{
    int ans;
    int fd;
    extern int errno;

    ans = (fd = open(path,1)) == -1 && errno == EISDIR;
    if( fd != -1 && close(fd) == -1 )
	return(-1);
    return(ans ? 1 : 0);
}

int connect_directly_to_toolkits (char* toolkits) {
  int sv[2];
  int exit_status;
  extern char **environ;
  // made this static because of the problem on SunOS 4.0 that
  // seemed to trash the parent variable in the parent thread!
  static int parent_fd;
  static int child_fd;
  if (!socketpair(AF_UNIX,SOCK_STREAM,0,sv)) {
    int pid;
#ifndef NOCHLD
    void (*old_handler)();
#endif
    parent_fd = sv[0]; child_fd = sv[1];
    fprintf(stderr, "child fd = %d, parent fd = %d\n",
	    child_fd, parent_fd);
    fcntl(parent_fd, F_SETFD, 1);
#ifndef NOCHLD
    // reset the SIGCHLD signal handler because Lucid seems to
    // catch it and gets all sorts of wrong ideas
    old_handler = signal(SIGCHLD, SIG_DFL);
#endif
#ifdef NOVFORK
    pid = fork ();
#else
    pid = vfork ();
#endif
    switch (pid) {
    case 0: {
      // child case -- fork a grandchild with the clm-server to
      // prevent zombies when closing the socket
#ifdef NOVFORK
      pid = fork ();
#else
      pid = vfork ();
#endif
      switch (pid) {
      case 0: {
	// grandchild case
	char x[100];
	char *argv[3];
	argv[0] = toolkits;
	argv[1] = x;
	argv[2] = (char*)NULL;
	{
	  // int i = getpid();
	  // set calling process PGID to its process ID
	  setpgrp();
	}
	sprintf(x, "%d", child_fd);
	execve(toolkits, argv, environ);
	// execve doesn't return on success, so we should never get
	// here.
	perror("exece:");
	return -1;
      }
      case -1 : {
	perror("fork:");
	_exit(-1);
      }
      default:
	// child case
	_exit(0);
      }
    }
    case -1 : {
      perror("fork:");
#ifndef NOCHLD
      signal (SIGCHLD, old_handler);
#endif
      return -1;
    }
    default:
      close (child_fd);
      // wait for child to exit, if there is no child, set exit_status
      // to 0, i.e. success.
      exit_status = 0;
      waitpid(pid, &exit_status, 0);
#ifndef NOCHLD
      signal (SIGCHLD, old_handler);
#endif
      if (exit_status == 0) {
	return parent_fd;
      } else {
	return -1;
      }
    }
  } else {
    perror("socketpair");
    return  -1;
  }
}
