/* Header: */
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <math.h>
#include <errno.h>
#include <gc.h>
#include "Rts.h"

/* Program argument handling */
int global_argc;
char **global_argv;
void getProgArgv(int *argc, char ***argv)
{
    *argc = global_argc;
    *argv = global_argv;
}

/* Panic function */
void panic(char *str)
{
  puts("====================== LHC Internal Error =======================");
  puts(str);
  fflush(stdout);
  fflush(stderr);
  exit(1);
}

/* Converting words to doubles */
typedef union { float d; unit *w; } DoubleOrUnit;
float wordToDouble(unit *x) {
  DoubleOrUnit u;
  u.w = x;
  return u.d;
}
unit *doubleToWord(float x) {
  DoubleOrUnit u;
  u.d = x;
  return u.w;
}

/* Memory allocation */
void* alloc(int size) { return GC_MALLOC(size); }

/*
unit *rts_newArray(unit *ptr, unit value, unit size)
{
    unit i;
    for(i = 0; i < size; i++) ptr[i] = value;
    return ptr;
}
*/
