/* Header: */
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <math.h>
#include <errno.h>
#include <gc.h>

#include "tommath.h"

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
  puts(str);
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

int __hscore_get_errno(void)
{
    return errno;
}
ssize_t __hscore_PrelHandle_write(int fd, void *ptr, int offset, size_t count)
{
    return write(fd, ptr + offset, count);
}
void *__hscore_memcpy_dst_off(void *dest, int offset, void *src, size_t n)
{
    return memcpy(dest+offset, src, n);
}

unit *rts_newArray(unit *ptr, unit value, unit size)
{
    unit i;
    for(i = 0; i < size; i++) ptr[i] = value;
    return ptr;
}

void show_mp(char *str, mp_int *mp)
{
    char buf[1000];
    printf("%s: ", str);
    mp_toradix(mp, buf, 10);
    printf("%s\n", buf);
}

mp_int *lhc_mp_from_int(sunit i)
{
    mp_int *mp;
    mp = (mp_int*) GC_MALLOC(sizeof(mp_int));
    mp_init_set_int(mp,i);
    return mp;
}
int lhc_mp_get_int(mp_int *mp)
{
    return mp_get_int(mp);
}

mp_int *lhc_mp_mul(mp_int *a, mp_int *b)
{
    mp_int *mp;
    mp = (mp_int*) GC_MALLOC(sizeof(mp_int));
    mp_init(mp);
    mp_mul(a,b,mp);
    return mp;
}
mp_int *lhc_mp_add(mp_int *a, mp_int *b)
{
    mp_int *mp;
    mp = (mp_int*) GC_MALLOC(sizeof(mp_int));
    mp_init(mp);
    mp_add(a,b,mp);
    return mp;
}
mp_int *lhc_mp_sub(mp_int *a, mp_int *b)
{
    mp_int *mp;
    mp = (mp_int*) GC_MALLOC(sizeof(mp_int));
    mp_init(mp);
    mp_sub(a,b,mp);
    return mp;
}
mp_int *lhc_mp_gcd(mp_int *a, mp_int *b)
{
    mp_int *mp;
    mp = (mp_int*) GC_MALLOC(sizeof(mp_int));
    mp_init(mp);
    mp_gcd(a,b,mp);
    return mp;
}
mp_int *lhc_mp_quot(mp_int *a, mp_int *b)
{
    mp_int *mp;
    mp_int rem;
    mp = (mp_int*) GC_MALLOC(sizeof(mp_int));
    mp_init(mp);
    mp_init(&rem);
    mp_div(a,b,mp,&rem);
    return mp;
}
mp_int *lhc_mp_rem(mp_int *a, mp_int *b)
{
    mp_int *mod;
    mod = (mp_int*) GC_MALLOC(sizeof(mp_int));
    mp_init(mod);
    mp_mod(a,b,mod);
    return mod;
}
mp_int *lhc_mp_abs(mp_int *a)
{
    mp_int *mp;
    mp = (mp_int*) GC_MALLOC(sizeof(mp_int));
    mp_init(mp);
    mp_abs(a,mp);
    return mp;
}
mp_int *lhc_mp_negate(mp_int *a)
{
    mp_int *mp;
    mp = (mp_int*) GC_MALLOC(sizeof(mp_int));
    mp_init(mp);
    mp_neg(a,mp);
    return mp;
}

sunit lhc_mp_cmp(mp_int *a, mp_int *b)
{
    return mp_cmp(a,b);
}

/*
int lhc_mp_cmp(mp_int *a, mp_int *b
foreign import ccall unsafe "lhc_mp_cmp" mp_cmp :: Mp_int -> Mp_int -> Int#
foreign import ccall unsafe "lhc_mp_get_int" mp_get_int :: Mp_int -> Int#

foreign import ccall unsafe "lhc_mp_from_int" mp_from_int :: Int# -> Mp_int
foreign import ccall unsafe "lhc_mp_mul" mp_mul :: Mp_int -> Mp_int -> Mp_int
foreign import ccall unsafe "lhc_mp_add" mp_add :: Mp_int -> Mp_int -> Mp_int
foreign import ccall unsafe "lhc_mp_sub" mp_sub :: Mp_int -> Mp_int -> Mp_int
foreign import ccall unsafe "lhc_mp_or" mp_or :: Mp_int -> Mp_int -> Mp_int
foreign import ccall unsafe "lhc_mp_and" mp_and :: Mp_int -> Mp_int -> Mp_int
foreign import ccall unsafe "lhc_mp_xor" mp_xor :: Mp_int -> Mp_int -> Mp_int
foreign import ccall unsafe "lhc_mp_gcd" mp_gcd :: Mp_int -> Mp_int -> Mp_int
foreign import ccall unsafe "lhc_mp_lcm" mp_lcm :: Mp_int -> Mp_int -> Mp_int
foreign import ccall unsafe "lhc_mp_quot" mp_quot :: Mp_int -> Mp_int -> Mp_int
foreign import ccall unsafe "lhc_mp_rem" mp_rem :: Mp_int -> Mp_int -> Mp_int
foreign import ccall unsafe "lhc_mp_abs" mp_abs :: Mp_int -> Mp_int
foreign import ccall unsafe "lhc_mp_negate" mp_negate :: Mp_int -> Mp_int
*/

/*
int global;

int fn(int i) {
  switch(i) {
  case 0:
    {
      global=global;
      int y=10;
      return y;
    }
  case 1:
    return (unit) doubleToWord(10);
  default:
    return 2;
  }
}
*/

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
