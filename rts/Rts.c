/* Header: */
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <math.h>
#include <errno.h>
#include <gc.h>
#include "Rts.h"

typedef unsigned long u64;
typedef unsigned int u32;
typedef unsigned short u16;
typedef unsigned char u8;
typedef signed long s64;
typedef signed int s32;
typedef signed short s16;
typedef signed char s8;

typedef u32 unit;
typedef s32 sunit;

int global_argc;
char **global_argv;

void getProgArgv(int *argc, char ***argv)
{
    *argc = global_argc;
    *argv = global_argv;
}

void panic(char *str)
{
  puts("====================== LHC Internal Error =======================");
  puts(str);
  fflush(stdout);
  fflush(stderr);
  exit(1);
}

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

ssize_t __hscore_PrelHandle_write(int fd, void *ptr, int offset, size_t count)
{
    return write(fd, ptr + offset, count);
}

unit *rts_newArray(unit *ptr, unit value, unit size)
{
    unit i;
    for(i = 0; i < size; i++) ptr[i] = value;
    return ptr;
}

void hs_free_stable_ptr(HsStablePtr sp) 
{
  panic("hs_free_stable_ptr not implemented");
}


#define BLOCK_SIZE (4096)
void* alloc(int size) { return GC_MALLOC(size); }

// Block allocation leads to less total allocations but higher total residency.
// The higher residency makes GCing a lot slower.
/*
void* alloc(int size)
{
    static void *p = NULL, *limit = NULL;
    void *t;
    int max;
    if (p==NULL) {
        p = GC_MALLOC(BLOCK_SIZE);
        limit = p + BLOCK_SIZE;
    }
    if (p+size > limit) {
        max = BLOCK_SIZE > size ? BLOCK_SIZE : size;
        p = GC_MALLOC(max);
        limit = p + max;
    }
    t = p;
    p += size;
    return t;
}
*/

