

#include <unistd.h>
#include "unixsocket.h"
#include <stdio.h>
#include "io.h"

#define TOOLKIT_PATH "/home/rett/dev/common-lisp/cl-motif/" \
  "cl-motif.git/clm/server/clm-server"

int main(int argc, char** argv) {
  int socet_fd = connect_directly_to_toolkits (TOOLKIT_PATH);
  printf("sending integer...\n");
  SendInteger(socet_fd, 4);
  printf("integer sent...\n");
  for(;;){
    sleep(1);
    SendInteger(socet_fd, 4);
    printf("client still alive...");
  };
  return 0;
}
