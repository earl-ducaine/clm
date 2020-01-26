static char sccsid[] = "@(#)td.c	1.8 9/9/93";

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
 *
 * Fixes for HP/UX: Dan Haug
 *                  Lockheed Austin Division
 *                  aihaug@AUSTIN.LOCKHEED.COM
 *
 * A fix to get rid of <defunct> processes was done by
 * Steve Strassman from MIT (straz@media-lab.media.mit.edu)
 *
 * Additional command line parameters by Chris Richardson, Franz Inc.
 * (cer@franz.com)
 */

#include <stdio.h>
#include <sys/types.h>
#include <sys/socket.h>

#ifdef UNIXCONN
#include <sys/un.h>
#endif

#include <netinet/in.h>
#include <signal.h>
#include <sys/wait.h>
#include <errno.h>

#ifdef rs6000
#include <sys/m_wait.h>
#endif

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Shell.h>

#include <Xm/RowColumn.h>
#include <Xm/Label.h>
#include <Xm/PushB.h>

#ifdef UNIXCONN
int unix_socket = 0;
#endif

#ifndef TD_RETRIES
#define TD_RETRIES 30
#endif

int inet_socket = 0;
XtInputId unixId, inetId;
int *connection_table, table_size;
int client_desc;
char *server_path;

/* If Null then do not create a unix domain */
char *socket_path = SOCKET_PATH;

/* Default value for the socket port */

int socket_port =  SOCKET_PORT;

/* TChild struct contains information about fork()'ed children */

typedef struct t_child TChild;

struct t_child {
    int     pid;
    int     selected;
    Widget  widget;
    char   *child_name;
    TChild *next;
    TChild *prev;
};

TChild child_list = {0,0,NULL,NULL,&child_list,&child_list};

/* controls whether we connect to a server and display stuff */

int display_status = 1 ;
Display *display;
Widget shell, outer_form, quit_button, kill_button, child_box, c_label;

static char socket_str[4];
static char *args[3] = { SERVER_PATH, socket_str, NULL };
extern char **environ;

#ifdef TD_DONT_DUMP
int dont_dump = TD_DONT_DUMP;
#else
int dont_dump = 0;
#endif

void fork_server(client_data,sock,id)
caddr_t client_data;
int *sock;
XtInputId *id;
{
    int pid;
    int desc;
    struct sockaddr sa;
    int sa_len = sizeof(struct sockaddr);
    int i;

    if( (desc = accept(*sock, &sa, &sa_len)) < 0 ) {
	if( errno == EINTR )
	    return;
	perror("accept");
	close_sockets();
    }

    switch( pid = fork() ) {
	case 0:  /* child */
	    /* Special hack to clean up Xt things .... */
	    /* close all open file descriptors except 0..2 and socket */
#ifdef hpux
	    for(i=(FD_SETSIZE-1); i>2; i--)
#else
	    for(i=getdtablesize()-1; i>2; i--)
#endif hpux
		if( i != desc )
		    close(i);
            sprintf(socket_str,"%1d",desc);
	    execve( server_path, args, environ);
	    perror("execve");
	    close_sockets();
	    /* NOTREACHED */
	case -1:  /* fork() failed */
	    perror("fork");
	    return;
	default:  /* parent */
	    if (display_status)
		add_child(pid,&sa,sa_len);
	    close(desc);
	    return;
    }
}

void toggle_it(widget,client_data,call_data)
Widget widget;
caddr_t client_data;
caddr_t call_data;
{
    TChild *child = (TChild *)client_data;
    Arg arg[1];

    child->selected = 1 - child->selected;
    child->child_name[0] = ( child->selected ? '*' : ' ' ) ;
    XtSetArg(arg[0],XmNlabelString,
	     XmStringLtoRCreate(child->child_name,XmSTRING_DEFAULT_CHARSET));
    XtSetValues(widget,arg,1);
}

add_child(pid,sa,sa_len)
int pid;
struct sockaddr *sa;
int sa_len;
{
    TChild *new;
    char *child_name;
    Arg arg[1];

    if( (new = (TChild *)malloc(sizeof(TChild))) == NULL ) {
	perror("malloc");
	close_sockets();
    }
    if( (new->child_name = (char *)malloc(100)) == NULL ) {
	perror("malloc");
	close_sockets();
    }

    new->pid = pid;
    new->next = child_list.next;
    new->prev = &child_list;
    new->selected = 0;
    sprintf(new->child_name, "  %s:%6d ", 
	    sa->sa_family == AF_UNIX ? "UNIX" : "INET", pid );
    XtSetArg(arg[0], XmNlabelString,
	     XmStringLtoRCreate(new->child_name,XmSTRING_DEFAULT_CHARSET));
    new->widget = XtCreateManagedWidget("child",xmPushButtonWidgetClass,
					child_box, arg, 1 );
    XtAddCallback(new->widget,XmNactivateCallback,toggle_it,new);
    XtRealizeWidget(new->widget);
    child_list.next->prev = new;
    child_list.next = new;
}

void delete_child(pid)
int pid;
{
    TChild *child;

    for( child = child_list.next; child != &child_list; child = child->next ) {
	if( child->pid == pid ) {
	    XtDestroyWidget(child->widget);
	    child->prev->next = child->next;
	    child->next->prev = child->prev;
	    free(child);
	    return;
	}
    }
    fprintf(stderr,"Can't find child %d\n",pid);
    fflush(stderr);
}
	    
reset_sig_child_handler()
{
    int sigchld_handler();

    signal(SIGCHLD, sigchld_handler);
}

int sigchld_handler(sig,code,scp,addr)
int sig,code;
struct sigcontext *scp;
char *addr;
{
    int pid;
    union wait w;

    if( (pid = wait3(&w.w_status,WNOHANG,NULL)) == -1 )
	perror("wait3");
    if( pid == 0 ) {
	fprintf(stderr,"wait3: nothing to report (PID == 0)\n");
	fflush(stderr);
    }
    else {
	if (display_status)
	    delete_child(pid);
    }
    reset_sig_child_handler();
}

close_sockets() 
{
    extern int errno;

    if( ! dont_dump )
        fprintf(stderr, "errno = %d\n", errno);
#ifdef UNIXCONN
    if (socket_path) {
	close(unix_socket);
	unlink(socket_path);
    }
#endif
    if (socket_port) 
	close(inet_socket);
    if( dont_dump )
	exit(0);
    fprintf(stderr,"Dumping core ...");
    fflush(stderr);
    abort();
}

void quit_toolkitd(widget,client_data,call_data)
Widget widget;
caddr_t client_data;
caddr_t call_data;
{
    dont_dump = 1;
    close_sockets();
}

void kill_client(widget,client_data,call_data)
Widget widget;
caddr_t client_data;
caddr_t call_data;
{
    TChild *child;

    for(child = child_list.next; child != &child_list; ) {
	child = child->next;
	if( child->prev->selected ) {
	    kill(child->prev->pid,SIGINT);
	    /*delete_child(child->pre->pid);*/
	}
    }
}



main(argc,argv) 
int argc;
char **argv;
{
#ifdef UNIXCONN
    struct sockaddr_un sa_un;
#endif
    struct sockaddr_in sa_in;
    long mask;
#ifdef UNIXCONN
    long unix_socket_mask = 0;
#endif
    long inet_socket_mask = 0;
    int num_desc;
    int ns, retries;
    Arg arg[2];
    XtAppContext app_context;
    
    server_path = SERVER_PATH;

    /* Look for:
       -server pathname 
         where to find the executable?
       -tcp port_number
          which port number to use
	  zero means none
       -unix filename
          pathname for the unix domain socket.
	  by default there is none
       -nostatus 
          Suppress status window.
	  */
    
{
    int i = 1;
    for (i = 1 ; i < argc ; i++) {
	/*printf("argv is %d,%s\n", argc,argv[i]);*/
	if (!strcmp(argv[i], "-server")) {
            server_path = (char *)XtMalloc (strlen(argv[++i]) + 1);
	    strcpy(server_path, argv[i]);
	}
	else if (!strcmp(argv[i], "-tcp")) {
	    sscanf(argv[++i],"%d", &socket_port);
	}
	else if (!strcmp(argv[i], "-nostatus")) {
	    display_status = 0;
	}
	else if (!strcmp(argv[i], "-unix")) {
	    socket_path = argv[++i];
	}
	else if (!strcmp(argv[i], "-nounix")) {
	    socket_path = NULL;
	}
        else if (!strcmp(argv[i], "-help")) {
            printf ("Usage: td [-server pathname] [-tcp socket_no]\n");
            printf ("          [-unix socket_path] [-nounix] [-nostatus]\n");
            exit (1);
        }
        else if (i == 1) {
            server_path = (char *)XtMalloc (strlen(argv[i]) + 1);
            strcpy (server_path, argv[i]);
	}
    }}	    
		
	
    /* Change the process group */

     { int i = getpid ();
       setpgid (i,i); }

    /* Create one unix domain socket and one internet socket */

#ifdef UNIXCONN
    if (socket_path)
	if( (unix_socket = socket(AF_UNIX, SOCK_STREAM, 0)) < 0 ) {
	    perror("socket");
	    exit(1);
	}
#endif
    if (socket_port)
	if( (inet_socket = socket(AF_INET, SOCK_STREAM, 0)) < 0 ) {
	    perror("socket");
	    close_sockets();
	}

#ifdef UNIXCONN
    /* Bind unix socket to a path in the directory hierarchie */
    if (socket_path) {
	sa_un.sun_family = AF_UNIX;
	strcpy(sa_un.sun_path, socket_path);

	for( retries = 0; retries <= TD_RETRIES; retries++ ) {
	    if( bind(unix_socket,&sa_un,strlen(SOCKET_PATH)+2) < 0 ) {
		if( errno == EADDRINUSE ) {
		    if( retries == 0 )
			unlink(socket_path);  /* try to remove clm_socket */
		    else
			perror("Unix domain bind");
		}
		if( retries == TD_RETRIES ) {
		    perror("Bind");
		    close_sockets();
		}
		else {
		    sleep(1);
		}
	    }
	    else {
		break;
	    }
	}
    }
#endif

    /* Bind the internet socket to an internet adress */
/*printf("socket port is %d\n", socket_port);*/

    if (socket_port) {
	bzero((char *)&sa_in, sizeof(sa_in));
	sa_in.sin_family = AF_INET;
	sa_in.sin_port = htons(socket_port);
	sa_in.sin_addr.s_addr = htonl(INADDR_ANY);

	for( retries = 0; retries <= TD_RETRIES; retries++ ) {
	    if( bind(inet_socket,(struct sockaddr *)&sa_in,
		                 sizeof(sa_in)) < 0 ) {
		if( errno == EADDRINUSE )
		    perror("Internet domain bind");
		if(retries == TD_RETRIES) {
		    perror("Bind");
		    close_sockets();
		}
		else {
		    sleep(1);
		}
	    }
	    else {
		break;
	    }
	}
    }
    /* Set up signal handlers to close down sockets properly */

    signal(SIGCHLD,sigchld_handler);
     signal(SIGHUP,close_sockets);
    signal(SIGINT,close_sockets);
    signal(SIGQUIT,close_sockets);

    /* Initialize queues for sockets */

#ifdef UNIXCONN
    if (socket_path)
	if( listen(unix_socket,5) < 0 ) {
	    perror("listen");
	    close_sockets();
	}
#endif
    if (socket_port)
	if( listen(inet_socket,5) < 0 ) {
	    perror("listen");
	    close_sockets();
	}

    /* Now wait for clients */

#ifdef UNIXCONN
    if (socket_path)
    unix_socket_mask =  (1 << unix_socket);
#endif
    if (socket_port)
    inet_socket_mask =  (1 << inet_socket);



    if (display_status) {
	XtToolkitInitialize();
	app_context = XtCreateApplicationContext();
    
	if( (display = XtOpenDisplay(app_context,NULL,APPNAME, APPCLASS,
				     NULL,0,&argc,argv)) == NULL) {
	    perror("XtOpenDisplay");
	    close_sockets();
	}

	XtSetArg(arg[0],XtNallowShellResize,True);
	shell = XtAppCreateShell(APPNAME, APPCLASS, applicationShellWidgetClass,
				 display, arg, 1 );

	outer_form = 
	    XtCreateManagedWidget("row-column",xmRowColumnWidgetClass,shell,arg,1);

	quit_button =
	    XtCreateManagedWidget("Quit",xmPushButtonWidgetClass,outer_form,NULL,0);
	XtAddCallback(quit_button, XmNactivateCallback, quit_toolkitd, NULL );

	kill_button =
	    XtCreateManagedWidget("Kill",xmPushButtonWidgetClass,outer_form,arg,0);
	XtAddCallback(kill_button, XmNactivateCallback, kill_client, NULL);

	child_box =
	    XtCreateManagedWidget("child_box",xmRowColumnWidgetClass,outer_form,arg,0);

	XtSetArg(arg[0],XmNborderWidth,0);
	c_label =
	    XtCreateManagedWidget("Clients",xmLabelWidgetClass,child_box,arg,1);
    
	XtRealizeWidget(shell);
#ifdef UNIXCONN
	if (socket_path)
	    XtAppAddInput(app_context, unix_socket,XtInputReadMask,fork_server,NULL);
#endif
	if (socket_port)
	    XtAppAddInput(app_context, inet_socket,XtInputReadMask,fork_server,NULL);

	XtAppMainLoop(app_context);
    }
    else {
	while (1) {
	    int fdset = inet_socket_mask | unix_socket_mask;
	    int n;
	    /*printf("calling select %d\n", fdset);*/
	    n = select(32, &fdset, NULL, NULL, NULL);
	    /*printf("select returned %d\n",n);*/
	    if ((n < 0)) {
                if ( (errno == EINTR))
                   continue;
                else {
                   perror("select");
		   exit(0);}
	    }
/*	    printf("masks are %d %d %d\n", fdset, 
		   inet_socket_mask, 
		   unix_socket_mask); */
	    if (socket_path &&  (unix_socket_mask & fdset)) {
		/*printf("starting unix socket\n");*/
		fork_server(NULL, &unix_socket);
	    }
	    if  (socket_port && (inet_socket_mask & fdset)) {
		/*printf("starting inet socket\n");*/
		fork_server(NULL,&inet_socket);
	    }
		
	}
    }
}

