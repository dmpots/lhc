/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2004
 *
 * Various C datatypes used in the run-time system.  This is the
 * lowest-level include file, after ghcconfig.h and RtsConfig.h.
 *
 * Do not #include this file directly: #include "Rts.h" instead.
 *
 * To understand the structure of the RTS headers, see the wiki:
 *   http://hackage.haskell.org/trac/ghc/wiki/Commentary/SourceTree/Includes
 *
 * NOTE: assumes #include "ghcconfig.h"
 * 
 * Works with or without _POSIX_SOURCE.
 *
 * WARNING: Keep this file, MachDeps.h, and HsFFI.h in synch!
 *
 * ---------------------------------------------------------------------------*/

#ifndef STGTYPES_H
#define STGTYPES_H

/*
 * This module should define types *only*, all beginning with "Stg".
 *
 * Specifically:

	StgInt8,  16, 32, 64
	StgWord8, 16, 32, 64
	StgChar, StgFloat, StgDouble

	***** All the same size (i.e. sizeof(void *)): *****
	StgPtr			Basic pointer type
	StgWord			Unit of heap allocation
	StgInt			Signed version of StgWord
	StgAddr			Generic address type
	
	StgBool, StgVoid, StgPtr, StgOffset, 
	StgCode, StgStablePtr, StgFunPtr,
	StgUnion.
 */

/*
 * First, platform-dependent definitions of size-specific integers.
 * Assume for now that the int type is 32 bits.
 * NOTE: Synch the following definitions with MachDeps.h!
 * ToDo: move these into a platform-dependent file.
 */

typedef signed   char            StgInt8;
typedef unsigned char            StgWord8;

typedef signed   short           StgInt16;
typedef unsigned short           StgWord16;

#if SIZEOF_LONG == 4
typedef signed   long            StgInt32;
typedef unsigned long            StgWord32;
#elif SIZEOF_INT == 4
typedef signed   int             StgInt32;
typedef unsigned int             StgWord32;
#else
#error GHC untested on this architecture: sizeof(int) != 4
#endif

#if SIZEOF_LONG == 8
typedef signed   long          StgInt64;
typedef unsigned long          StgWord64;
#elif defined(__MSVC__)
typedef __int64                StgInt64;
typedef unsigned __int64       StgWord64;
#elif SIZEOF_LONG_LONG == 8
typedef signed long long int   StgInt64;
typedef unsigned long long int StgWord64;
#else
#error cannot find a way to define StgInt64
#endif

/*
 * Define the standard word size we'll use on this machine: make it
 * big enough to hold a pointer.
 *
 * It's useful if StgInt/StgWord are always the same as long, so that
 * we can use a consistent printf format specifier without warnings on
 * any platform.  Fortunately this works at the moement; if it breaks
 * in the future we'll have to start using macros for format
 * specifiers (c.f. FMT_StgWord64 in Rts.h).
 */

#if SIZEOF_VOID_P == 8
typedef StgInt64           StgInt;
typedef StgWord64          StgWord;
typedef StgInt32           StgHalfInt;
typedef StgWord32          StgHalfWord;
#else
#if SIZEOF_VOID_P == 4
typedef StgInt32           StgInt; 
typedef StgWord32          StgWord;
typedef StgInt16           StgHalfInt;
typedef StgWord16          StgHalfWord;
#else
#error GHC untested on this architecture: sizeof(void *) != 4 or 8
#endif
#endif

#define W_MASK  (sizeof(W_)-1)

/*
 * Other commonly-used STG datatypes.
 */

typedef void*              StgAddr;
typedef StgWord32          StgChar;
typedef int                StgBool;
typedef float		   StgFloat;
typedef double		   StgDouble;
typedef StgWord*           StgPtr;           /* heap or stack pointer */
typedef StgWord volatile*  StgVolatilePtr;   /* pointer to volatile word   */
typedef StgWord            StgOffset;        /* byte offset within closure */
typedef StgWord8 	   StgCode;  	     /* close enough */
typedef void*		   StgStablePtr;
typedef StgWord8*          StgByteArray;

/*
  Types for the generated C functions
  take no arguments
  return a pointer to the next function to be called
  use: Ptr to Fun that returns a Ptr to Fun which returns Ptr to void

  Note: Neither StgFunPtr not StgFun is quite right (that is, 
  StgFunPtr != StgFun*).  So, the functions we define all have type
  StgFun but we always have to cast them to StgFunPtr when we assign
  them to something.
  The only way round this would be to write a recursive type but
  C only allows that if you're defining a struct or union.
*/

typedef void  *(*(*StgFunPtr)(void))(void);
typedef StgFunPtr StgFun(void);

#endif /* STGTYPES_H */
