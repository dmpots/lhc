
#include <stdlib.h>
#include <sys/select.h>


/* waitRead# */
int waitRead(int fd) {
  fd_set fdSet;
  FD_ZERO(&fdSet);
  FD_SET(fd, &fdSet);
  return select(fd, &fdSet, NULL, NULL, NULL);
}




