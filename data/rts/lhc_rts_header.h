
// lhc_rts_header.h

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <wchar.h>
#include <limits.h>
#include <locale.h>
#include <math.h>
#include <assert.h>
#include <float.h>
#include <sys/times.h>
#include <setjmp.h>


// #define our options

#define _LHC_GC_NONE  0
#define _LHC_GC_BOEHM 1


#ifndef _LHC_GC
#define _LHC_GC _LHC_GC_NONE
#endif

#ifndef _LHC_PROFILE
#define _LHC_PROFILE 0
#endif

#ifndef _LHC_DEBUG
#ifdef NDEBUG
#define _LHC_DEBUG 0
#else
#define _LHC_DEBUG 1
#endif
#endif


// GNU attributes

#ifdef __GNUC__
#define A_ALIGNED  __attribute__ ((aligned))
#define A_CONST    __attribute__ ((const))
#define A_MALLOC   __attribute__ ((malloc))
#define A_MAYALIAS __attribute__ ((__may_alias__))
#define A_NORETURN __attribute__ ((noreturn))
#define A_PURE     __attribute__ ((pure))
#define A_UNUSED   __attribute__ ((unused))
#ifdef __i386__
#define A_REGPARM __attribute__ ((fastcall))
#else
#define A_REGPARM
#endif
#define A_STD    A_REGPARM

#else
#define A_ALIGNED
#define A_CONST
#define A_MALLOC
#define A_MAYALIAS
#define A_NORETURN
#define A_PURE
#define A_UNUSED
#define A_STD
#endif

// these should be enabled with newer versions of gcc
#define A_HOT
#define A_COLD
#define A_FALIGNED

#define STR(s) #s
#define XSTR(s) STR(s)
#define ALIGN(a,n) ((n) - 1 + ((a) - ((n) - 1) % (a)))



