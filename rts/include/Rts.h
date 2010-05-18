#ifndef __GUARD_LHC_RTS_H
#define __GUARD_LHC_RTS_H
#include "HsFFI.h"
#include "longlong.h"
#include "ghc/RtsGhc.h"
#include "ghc/StgPrimFloat.h"

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

/* External global variables */
extern int global_argc;
extern char **global_argv;

/* Forward definitions */
void* alloc(int size);
void panic(char *str);
float wordToDouble(unit *x);
unit *doubleToWord(float x);
#endif
