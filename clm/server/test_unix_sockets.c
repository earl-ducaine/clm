

#include "unixsocket.h"

#define TOOLKIT_PATH "/home/rett/dev/common-lisp/cl-motif/" \
  "cl-motif.git/clm/server/clm-server"

int main(int argc, char** argv) {
  connect_directly_to_toolkits (TOOLKIT_PATH);
  for(;;){};
  return 0;
}
