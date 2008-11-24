module RawFiles where


-- | Generated from data\/HsFFI.h on Tue Nov 18 04:44:37 CET 2008
{-# NOINLINE hsffi_h #-}
hsffi_h :: String
hsffi_h = "\
 \/* HsFFI.h for lhc */\n\
 \\n\
 \#ifndef _LHC_HSFFI_H\n\
 \#define _LHC_HSFFI_H\n\
 \\n\
 \#include <inttypes.h>\n\
 \\n\
 \\n\
 \typedef int32_t HsInt;\n\
 \typedef int8_t  HsInt8;\n\
 \typedef int16_t HsInt16;\n\
 \typedef int32_t HsInt32;\n\
 \typedef int64_t HsInt64;\n\
 \\n\
 \typedef uint32_t HsWord;\n\
 \typedef uint8_t  HsWord8;\n\
 \typedef uint16_t HsWord16;\n\
 \typedef uint32_t HsWord32;\n\
 \typedef uint64_t HsWord64;\n\
 \\n\
 \typedef uint32_t HsChar;\n\
 \typedef int HsBool;\n\
 \\n\
 \typedef double HsDouble;\n\
 \typedef float HsFloat;\n\
 \\n\
 \typedef void *HsPtr;\n\
 \typedef void (*HsFunPtr)(void);\n\
 \typedef void *HsStablePtr;\n\
 \\n\
 \#define HS_BOOL_FALSE 0\n\
 \#define HS_BOOL_TRUE 1\n\
 \\n\
 \void hs_init (int *argc, char **argv[]);\n\
 \void hs_exit (void);\n\
 \void hs_set_argv(int argc, char *argv[]);\n\
 \void hs_perform_gc(void);\n\
 \void hs_free_stable_ptr(HsStablePtr sp);\n\
 \void hs_free_fun_ptr(HsFunPtr fp);\n\
 \\n\
 \#endif\n\
 \"

-- | Generated from data\/rts\/lhc_rts.c on Tue Nov 18 04:44:37 CET 2008
{-# NOINLINE lhc_rts_c #-}
lhc_rts_c :: String
lhc_rts_c = "\
 \\n\
 \static void _amain(void);\n\
 \static void lhc_arch_assert(void);\n\
 \static int lhc_argc;\n\
 \static char **lhc_argv;\n\
 \static char *lhc_progname;\n\
 \static jmp_buf lhc_uncaught;\n\
 \\n\
 \static HsInt lhc_stdrnd[2] A_UNUSED = { 1 , 1 };\n\
 \\n\
 \#if _LHC_PROFILE\n\
 \\n\
 \static uintmax_t lhc_prof_function_calls;\n\
 \static uintmax_t lhc_prof_case_statements;\n\
 \static uintmax_t lhc_prof_updates;\n\
 \\n\
 \#define lhc_update_inc()   lhc_prof_updates++\n\
 \#define lhc_function_inc() lhc_prof_function_calls++\n\
 \#define lhc_case_inc()     lhc_prof_case_statements++\n\
 \\n\
 \#else\n\
 \\n\
 \#define lhc_update_inc()    do { } while(0)\n\
 \#define lhc_function_inc()  do { } while(0)\n\
 \#define lhc_case_inc()      do { } while(0)\n\
 \\n\
 \#endif\n\
 \\n\
 \static void A_COLD\n\
 \lhc_print_profile(void) {\n\
 \        struct tms tm;\n\
 \        times(&tm);\n\
 \        if(!(_LHC_PROFILE || getenv(\"LHC_RTS_PROFILE\"))) return;\n\
 \\n\
 \        fprintf(stderr, \"\\n-----------------\\n\");\n\
 \        fprintf(stderr, \"Profiling: %s\\n\", lhc_progname);\n\
 \        fprintf(stderr, \"Command: %s\\n\", lhc_command);\n\
 \        fprintf(stderr, \"Complie: %s\\n\", lhc_c_compile);\n\
 \        fprintf(stderr, \"Version: %s\\n\\n\", lhc_version);\n\
 \        lhc_alloc_print_stats();\n\
 \        float cpt = (float)sysconf(_SC_CLK_TCK);\n\
 \        fprintf(stderr, \"User Time:   %.2fs\\n\", (float)tm.tms_utime/cpt);\n\
 \        fprintf(stderr, \"System Time: %.2fs\\n\", (float)tm.tms_stime/cpt);\n\
 \        fprintf(stderr, \"Total Time:  %.2fs\\n\", (float)(tm.tms_stime + tm.tms_utime)/cpt);\n\
 \\n\
 \#if _LHC_PROFILE\n\
 \        fprintf(stderr, \"\\nFunction Calls:   %llu\\n\", (unsigned long long)lhc_prof_function_calls);\n\
 \        fprintf(stderr, \"Case Statements:  %llu\\n\", (unsigned long long)lhc_prof_case_statements);\n\
 \        fprintf(stderr, \"Updates:          %llu\\n\", (unsigned long long)lhc_prof_updates);\n\
 \#endif\n\
 \        fprintf(stderr, \"-----------------\\n\");\n\
 \}\n\
 \\n\
 \\n\
 \static void A_NORETURN A_UNUSED A_COLD\n\
 \lhc_exit(int n) {\n\
 \        lhc_print_profile();\n\
 \        exit(n);\n\
 \}\n\
 \\n\
 \static void  A_NORETURN A_UNUSED  A_COLD\n\
 \lhc_error(char *s) {\n\
 \        fputs(s,stderr);\n\
 \        fputs(\"\\n\",stderr);\n\
 \        lhc_print_profile();\n\
 \        exit(255);\n\
 \}\n\
 \\n\
 \static void  A_NORETURN A_UNUSED  A_COLD\n\
 \lhc_case_fell_off(int n) {\n\
 \        fflush(stdout);\n\
 \        fprintf(stderr, \"\\n%s:%i: case fell off\\n\", __FILE__, n);\n\
 \        abort();\n\
 \}\n\
 \\n\
 \#define lhc_setjmp(jb) sigsetjmp(*(jmp_buf *)jb,0)\n\
 \#define lhc_longjmp(jb) siglongjmp(*(jmp_buf *)jb,1)\n\
 \\n\
 \struct lhc_continuation {\n\
 \    void *argument;\n\
 \    jmp_buf jump_buf;\n\
 \};\n\
 \\n\
 \#define prim_umaxbound(t) ((t)~((t)0))\n\
 \#define prim_maxbound(t) ((t)(~((t)1 << (sizeof(t)*8 - 1))))\n\
 \#define prim_minbound(t) ((t)(((t)1 << (sizeof(t)*8 - 1))))\n\
 \\n\
 \\n\
 \inline static int A_UNUSED\n\
 \lhc_utf8_getchar(void)\n\
 \{\n\
 \    return getchar_unlocked();\n\
 \}\n\
 \\n\
 \inline static int A_UNUSED\n\
 \lhc_utf8_getc(FILE *f)\n\
 \{\n\
 \    return getc_unlocked(f);\n\
 \}\n\
 \\n\
 \inline static int A_UNUSED\n\
 \lhc_utf8_putchar(int ch)\n\
 \{\n\
 \    return putchar_unlocked(ch);\n\
 \}\n\
 \\n\
 \inline static int A_UNUSED\n\
 \lhc_utf8_putc(int ch, FILE *f)\n\
 \{\n\
 \    return putc_unlocked(ch,f);\n\
 \}\n\
 \\n\
 \\n\
 \int  A_COLD\n\
 \main(int argc, char *argv[])\n\
 \{\n\
 \        /* A few random assertions about the architecture that the compiler\n\
 \         * assumes. should be true of any but the oddest of beasts.\n\
 \         */\n\
 \\n\
 \        assert(sizeof(HsPtr) == sizeof(HsFunPtr));\n\
 \        assert(sizeof(HsPtr) == sizeof(intptr_t));\n\
 \        assert(sizeof(HsPtr) == sizeof(uintptr_t));\n\
 \        assert(CHAR_BIT == 8);\n\
 \        assert(EOF == -1);\n\
 \\n\
 \        lhc_arch_assert();\n\
 \        lhc_malloc_init();\n\
 \        lhc_argc = argc - 1;\n\
 \        lhc_argv = argv + 1;\n\
 \        lhc_progname = argv[0];\n\
 \        setlocale(LC_ALL,\"\");\n\
 \        if (sigsetjmp(lhc_uncaught,0))\n\
 \                lhc_error(\"Uncaught Exception\");\n\
 \        else\n\
 \                _amain();\n\
 \        lhc_print_profile();\n\
 \        return 0;\n\
 \}\n\
 \\n\
 \\n\
 \\n\
 \\n\
 \"

-- | Generated from data\/rts\/lhc_rts_header.h on Tue Nov 18 04:44:37 CET 2008
{-# NOINLINE lhc_rts_header_h #-}
lhc_rts_header_h :: String
lhc_rts_header_h = "\
 \\n\
 \// lhc_rts_header.h\n\
 \\n\
 \#include <stdlib.h>\n\
 \#include <stdio.h>\n\
 \#include <string.h>\n\
 \#include <unistd.h>\n\
 \#include <wchar.h>\n\
 \#include <limits.h>\n\
 \#include <locale.h>\n\
 \#include <math.h>\n\
 \#include <assert.h>\n\
 \#include <float.h>\n\
 \#include <sys/times.h>\n\
 \#include <setjmp.h>\n\
 \\n\
 \\n\
 \// #define our options\n\
 \\n\
 \#define _LHC_GC_NONE  0\n\
 \#define _LHC_JGC      1\n\
 \#define _LHC_GC_BOEHM 2\n\
 \\n\
 \\n\
 \#ifndef _LHC_GC\n\
 \#define _LHC_GC _LHC_GC_NONE\n\
 \#endif\n\
 \\n\
 \#ifndef _LHC_PROFILE\n\
 \#define _LHC_PROFILE 0\n\
 \#endif\n\
 \\n\
 \#ifndef _LHC_DEBUG\n\
 \#ifdef NDEBUG\n\
 \#define _LHC_DEBUG 0\n\
 \#else\n\
 \#define _LHC_DEBUG 1\n\
 \#endif\n\
 \#endif\n\
 \\n\
 \\n\
 \// GNU attributes\n\
 \\n\
 \#ifdef __GNUC__\n\
 \#define A_ALIGNED  __attribute__ ((aligned))\n\
 \#define A_CONST    __attribute__ ((const))\n\
 \#define A_MALLOC   __attribute__ ((malloc))\n\
 \#define A_MAYALIAS __attribute__ ((__may_alias__))\n\
 \#define A_NORETURN __attribute__ ((noreturn))\n\
 \#define A_PURE     __attribute__ ((pure))\n\
 \#define A_UNUSED   __attribute__ ((unused))\n\
 \#ifdef __i386__\n\
 \#define A_REGPARM __attribute__ ((fastcall))\n\
 \#else\n\
 \#define A_REGPARM\n\
 \#endif\n\
 \#define A_STD    A_REGPARM\n\
 \\n\
 \#else\n\
 \#define A_ALIGNED\n\
 \#define A_CONST\n\
 \#define A_MALLOC\n\
 \#define A_MAYALIAS\n\
 \#define A_NORETURN\n\
 \#define A_PURE\n\
 \#define A_UNUSED\n\
 \#define A_STD\n\
 \#endif\n\
 \\n\
 \// these should be enabled with newer versions of gcc\n\
 \#define A_HOT\n\
 \#define A_COLD\n\
 \#define A_FALIGNED\n\
 \\n\
 \#define STR(s) #s\n\
 \#define XSTR(s) STR(s)\n\
 \#define ALIGN(a,n) ((n) - 1 + ((a) - ((n) - 1) % (a)))\n\
 \\n\
 \\n\
 \\n\
 \"

-- | Generated from data\/wsize.h on Tue Nov 18 04:44:37 CET 2008
{-# NOINLINE wsize_h #-}
wsize_h :: String
wsize_h = "\
 \#ifndef WSIZE_H\n\
 \#define WSIZE_H\n\
 \\n\
 \/*\n\
 \ * wsize.h\n\
 \ * define appropriate __WORDSIZE and __BYTE_ORDER macros\n\
 \ *\n\
 \ * always use operating systems headers rather than checking for architectures\n\
 \ * when possible. if adding new cases. Checking the CPU type should be a last\n\
 \ * resort.\n\
 \ *\n\
 \ */\n\
 \\n\
 \#include <limits.h>\n\
 \\n\
 \#ifdef __linux__\n\
 \#include<endian.h>\n\
 \#endif\n\
 \\n\
 \#ifndef __LITTLE_ENDIAN\n\
 \#define\x0009\&__LITTLE_ENDIAN\x0009\&1234\n\
 \#endif\n\
 \#ifndef __BIG_ENDIAN\n\
 \#define\x0009\&__BIG_ENDIAN\x0009\&4321\n\
 \#endif\n\
 \#ifndef __PDP_ENDIAN\n\
 \#define\x0009\&__PDP_ENDIAN\x0009\&3412\n\
 \#endif\n\
 \\n\
 \#ifndef __BYTE_ORDER\n\
 \#ifdef _BIG_ENDIAN\n\
 \#define __BYTE_ORDER __BIG_ENDIAN\n\
 \#elif defined(_LITTLE_ENDIAN)\n\
 \#define __BYTE_ORDER __LITTLE_ENDIAN\n\
 \#elif defined(__i386__)\n\
 \#define __BYTE_ORDER __LITTLE_ENDIAN\n\
 \#else\n\
 \#error Could not determine Byte Order\n\
 \#endif\n\
 \#endif\n\
 \\n\
 \#ifndef __WORDSIZE\n\
 \#ifdef WORD_BIT\n\
 \#define __WORDSIZE WORD_BIT\n\
 \#elif defined(__i386__)\n\
 \#define __WORDSIZE 32\n\
 \#elif defined(__x86_64__)\n\
 \#define __WORDSIZE 64\n\
 \#else\n\
 \#error Could not determine bitsize\n\
 \#endif\n\
 \\n\
 \#endif\n\
 \\n\
 \\n\
 \#ifdef TEST_WSIZE\n\
 \#include <stdio.h>\n\
 \int\n\
 \main(int argc, char *argv[])\n\
 \{\n\
 \    printf(\"__WORDSIZE:   %i\\n\", __WORDSIZE);\n\
 \    printf(\"__BYTE_ORDER: %i\\n\", __BYTE_ORDER);\n\
 \    return 0;\n\
 \}\n\
 \#endif\n\
 \\n\
 \#endif\n\
 \"

-- | Generated from data\/rts\/lhc_rts_alloc.c on Tue Nov 18 04:44:37 CET 2008
{-# NOINLINE lhc_rts_alloc_c #-}
lhc_rts_alloc_c :: String
lhc_rts_alloc_c = "\
 \\n\
 \// some default definitions\n\
 \\n\
 \#define lhc_malloc_whnf lhc_malloc\n\
 \#define lhc_malloc_suspension lhc_malloc\n\
 \#define lhc_malloc_atomic lhc_malloc\n\
 \#define lhc_malloc_atomic_whnf lhc_malloc_atomic\n\
 \#define lhc_malloc_sanity(p,t) (1)\n\
 \\n\
 \extern void _start,_end;\n\
 \\n\
 \#if _LHC_PROFILE\n\
 \\n\
 \#define BUCKETS 7\n\
 \\n\
 \static unsigned alloced[BUCKETS];\n\
 \static unsigned alloced_atomic[BUCKETS];\n\
 \\n\
 \static void\n\
 \alloc_count(int n,int atomic)\n\
 \{\n\
 \        n = n ? ((n - 1)/sizeof(void *)) + 1 : 0;\n\
 \        n = n > BUCKETS - 1 ? BUCKETS - 1 : n;\n\
 \        (atomic ? alloced_atomic : alloced)[n]++;\n\
 \}\n\
 \\n\
 \\n\
 \static void\n\
 \print_alloc_size_stats(void) {\n\
 \        char fmt[] = \"%10s %10s %10s %10s %10s\\n\";\n\
 \        char fmt2[] = \"%10u %10u %10u %10u %10u\\n\";\n\
 \        fprintf(stderr,fmt,\"Size\",\"Normal\",\"Atomic\",\"Total\",\"Accum\");\n\
 \        fprintf(stderr,fmt,\"----\",\"------\",\"------\",\"-----\",\"-----\");\n\
 \        unsigned accum = 0;\n\
 \        for(int i = 0; i < BUCKETS; i++) {\n\
 \                accum += alloced[i] + alloced_atomic[i];\n\
 \                fprintf(stderr,fmt2,i,alloced[i],alloced_atomic[i],alloced_atomic[i] + alloced[i], accum);\n\
 \        }\n\
 \}\n\
 \\n\
 \\n\
 \#else\n\
 \\n\
 \#define alloc_count(x,y)\n\
 \#define print_alloc_size_stats()\n\
 \\n\
 \#endif\n\
 \\n\
 \#if _LHC_GC == _LHC_GC_BOEHM\n\
 \\n\
 \#include <gc/gc.h>\n\
 \\n\
 \#define lhc_malloc GC_malloc\n\
 \#undef  lhc_malloc_atomic\n\
 \#define lhc_malloc_atomic GC_malloc_atomic\n\
 \#define lhc_free GC_free\n\
 \\n\
 \static inline void lhc_malloc_init(void) { GC_INIT(); }\n\
 \static inline void lhc_alloc_print_stats(void) { GC_dump(); }\n\
 \\n\
 \#elif _LHC_GC == _LHC_GC_NONE\n\
 \\n\
 \// memory allocated in 1MB chunks.\n\
 \#define LHC_MEM_CHUNK_SIZE (1 << 20)\n\
 \\n\
 \static char initial_chunk[LHC_MEM_CHUNK_SIZE];\n\
 \\n\
 \static void *lhc_current_chunk = initial_chunk;\n\
 \static unsigned mem_chunks,mem_offset;\n\
 \\n\
 \\n\
 \static inline void\n\
 \lhc_malloc_init(void) { return; }\n\
 \\n\
 \static void\n\
 \lhc_alloc_print_stats(void) {\n\
 \        fprintf(stderr, \"Memory Allocated: %u bytes\\n\", (LHC_MEM_CHUNK_SIZE*(mem_chunks)) + mem_offset);\n\
 \        print_alloc_size_stats();\n\
 \}\n\
 \\n\
 \static void\n\
 \lhc_malloc_grow(void) {\n\
 \        void *c = malloc(LHC_MEM_CHUNK_SIZE);\n\
 \        if(!c) {\n\
 \                fputs(\"Out of memory!\\n\",stderr);\n\
 \                abort();\n\
 \        }\n\
 \        mem_chunks++;\n\
 \        lhc_current_chunk = c;\n\
 \        mem_offset = 0;\n\
 \}\n\
 \\n\
 \static inline void * A_MALLOC\n\
 \lhc_malloc_basic(size_t n) {\n\
 \        n = ALIGN(sizeof(void *),n);\n\
 \        if (n > (LHC_MEM_CHUNK_SIZE - mem_offset))\n\
 \                lhc_malloc_grow();\n\
 \        void *ret = lhc_current_chunk + mem_offset;\n\
 \        mem_offset += n;\n\
 \        return ret;\n\
 \}\n\
 \\n\
 \\n\
 \#if _LHC_DEBUG\n\
 \\n\
 \#define lhc_malloc(n) lhc_malloc_debug(n,__LINE__,0)\n\
 \#undef lhc_malloc_atomic\n\
 \#define lhc_malloc_atomic(n) lhc_malloc_debug(n,__LINE__,1)\n\
 \\n\
 \static void * A_MALLOC\n\
 \lhc_malloc_debug(size_t n,int line,int atomic) {\n\
 \        alloc_count(n,atomic);\n\
 \        void *ret = lhc_malloc_basic(n + sizeof(uintptr_t));\n\
 \        *((uintptr_t *)ret) = line;\n\
 \        return ret + sizeof(uintptr_t);\n\
 \}\n\
 \\n\
 \#else\n\
 \\n\
 \static inline void * A_MALLOC\n\
 \lhc_malloc(size_t n) {\n\
 \        alloc_count(n,0);\n\
 \        return lhc_malloc_basic(n);\n\
 \}\n\
 \\n\
 \#undef lhc_malloc_atomic\n\
 \static inline void * A_MALLOC\n\
 \lhc_malloc_atomic(size_t n) {\n\
 \        alloc_count(n,1);\n\
 \        return lhc_malloc_basic(n);\n\
 \}\n\
 \\n\
 \\n\
 \#endif\n\
 \\n\
 \#elif _LHC_GC == _LHC_GC_JGC\n\
 \\n\
 \#error \"jgc not supported yet.\"\n\
 \\n\
 \#endif\n\
 \\n\
 \\n\
 \\n\
 \\n\
 \"

-- | Generated from data\/rts\/lhc_rts2.c on Tue Nov 18 04:44:37 CET 2008
{-# NOINLINE lhc_rts2_c #-}
lhc_rts2_c :: String
lhc_rts2_c = "\
 \\n\
 \#define ISLAZY(x)    (((uintptr_t)(x)) & 0x1)\n\
 \#define DETAG(x)     ((uintptr_t)(x) & ~0x3)\n\
 \#define GETTAG(x)    ((uintptr_t)(x) & 0x3)\n\
 \\n\
 \#define GETHEAD(x)   (NODEP(x)->head)\n\
 \#define NODEP(x)     ((node_t *)(x))\n\
 \#define DNODEP(x)    ((dnode_t *)(x))\n\
 \#define EVALTAG(fn)  (assert(((uintptr_t)(fn) & 0x3) == 0),(sptr_t)((uintptr_t)(fn) | P_LAZY))\n\
 \#define EVALTAGC(fn) ((sptr_t)((void *)(fn) + P_LAZY))\n\
 \#define EVALFUNC(fn) ((fptr_t)((uintptr_t)(fn) | P_FUNC))\n\
 \#define VALUE(n)     ((wptr_t)(((intptr_t)(n) << 2) | P_VALUE))\n\
 \#define GETVALUE(n)  ((intptr_t)(n) >> 2)\n\
 \#define ISVALUE(n)   (assert(!ISLAZY(n)), ((uintptr_t)(n) & 0x2))\n\
 \#define PROMOTE(n)   ((wptr_t)(n))\n\
 \#define DEMOTE(n)    ((sptr_t)(n))\n\
 \#define GETWHAT(x)   (GETTAG(x) == P_VALUE ? ((uintptr_t)(x) >> 16) : DNODEP(x)->what)\n\
 \\n\
 \#define SETWHAT(x,v)   (DNODEP(x)->what = (v))\n\
 \#define RAWWHAT(w)     (wptr_t)(((uintptr_t)w << 16) | P_VALUE)\n\
 \\n\
 \\n\
 \#define P_WHNF  0x0\n\
 \#define P_LAZY  0x1\n\
 \#define P_VALUE 0x2\n\
 \#define P_FUNC  0x3\n\
 \\n\
 \#define BLACK_HOLE ((fptr_t)0xDEADBEEF)\n\
 \\n\
 \\n\
 \/*@Internals\n\
 \\n\
 \# The Run Time System\n\
 \\n\
 \Lhc is very minimalist in that it does not have a precompiled run time system,\n\
 \but rather generates what is needed as part of the compilation process.\n\
 \However, we call whatever conventions and binary layouts used in the generated\n\
 \executable the run time system. Since lhc generates the code anew each time, it\n\
 \can build a different 'run time' based on compiler options, trading things like\n\
 \the garbage collector as needed or changing the closure layout when we know we\n\
 \have done full program optimization. This describes the 'native' layout upon\n\
 \which other conventions are layered.\n\
 \\n\
 \A basic value in lhc is represented by a 'smart pointer' of c type sptr_t. a\n\
 \smart pointer is the size of a native pointer, but can take on different roles\n\
 \depending on a pair of tag bits.\n\
 \\n\
 \smart pointers take on a general form as follows:\n\
 \\n\
 \    -------------------------\n\
 \    |    payload        | GL|\n\
 \    -------------------------\n\
 \\n\
 \      G - if set, then the garbage collector should not treat value as a pointer to be followed\n\
 \      L - lazy, this bit being set means the value is not in WHNF\n\
 \\n\
 \A raw sptr_t on its own in the wild can only take on one of the following values:\n\
 \\n\
 \    -------------------------\n\
 \    |    raw value      | 10|\n\
 \    -------------------------\n\
 \\n\
 \    -------------------------\n\
 \    |    whnf location  | 00|\n\
 \    -------------------------\n\
 \\n\
 \    -------------------------\n\
 \    |   lazy location   | 01|\n\
 \    -------------------------\n\
 \\n\
 \A raw value can be anything and not necessarily a pointer in general, a WHNF\n\
 \location is a pointer to some value in WHNF. The system places no restrictions\n\
 \on what is actually pointed to by a WHNF pointer, however the garbage collector\n\
 \in use may. In general, the back end is free to choose what to place in the raw\n\
 \value field or in what a WHNF points to with complete freedom. If an\n\
 \implementation sees the L bit is clear, it can pass on the smart pointer\n\
 \without examining it knowing the value is in WHNF.\n\
 \\n\
 \A lazy location points to a potential closure or an indirection to a WHNF\n\
 \value. The lazy location is an allocated chunk of memory that is at least\n\
 \one pointer long. the very first location in a closure must be one of the\n\
 \following.\n\
 \\n\
 \    -------------------------\n\
 \    | raw value or whnf  |X0|\n\
 \    -------------------------\n\
 \\n\
 \An evaluated value, interpreted exactly as above. one can always replace any occurance of a\n\
 \lazy location with an evaluated indirecton.\n\
 \\n\
 \    -------------------------\n\
 \    |    code pointer   | 11|\n\
 \    -------------------------\n\
 \    |     data ...          |\n\
 \\n\
 \This is something to evaluate, code pointer is a pointer to a function that takes\n\
 \the memory location as its only argument, the called function is in charge\n\
 \of updating the location if needed.\n\
 \\n\
 \note that it is invalid to have a lazy location point to another lazy\n\
 \location. there is only ever one level of indirection allowed, and only from\n\
 \lazy locations\n\
 \\n\
 \note that a partial application is just like any other value in WHNF as far\n\
 \as the above is concered. It happens to possibly contain a code pointer.\n\
 \\n\
 \*/\n\
 \\n\
 \\n\
 \/*\n\
 \ * type names\n\
 \ *\n\
 \ * sptr_t - a tagged smart pointer, may be a value, may be a pointer to a whnf or lazy location\n\
 \ * wptr_t - a value guarenteed to be in whnf\n\
 \ * fptr_t - a pointer to a whnf or a function pointer to something to evaluate, first value in a lazy location.\n\
 \ * what_t  - the discriminator of a discriminated union\n\
 \ *\n\
 \ */\n\
 \\n\
 \typedef struct node *  sptr_t;\n\
 \typedef struct dnode * wptr_t;\n\
 \typedef void *         fptr_t;\n\
 \typedef uintptr_t      what_t;\n\
 \\n\
 \\n\
 \typedef struct node {\n\
 \        fptr_t head;\n\
 \        sptr_t rest[];\n\
 \} A_MAYALIAS node_t;\n\
 \\n\
 \typedef struct dnode {\n\
 \        what_t what;\n\
 \        sptr_t rest[];\n\
 \} A_MAYALIAS dnode_t;\n\
 \\n\
 \#if _LHC_DEBUG\n\
 \\n\
 \// these ensure the type synonyms are available to the debugger\n\
 \uintptr_t _dummy1;\n\
 \node_t *_dummy2;\n\
 \dnode_t *_dummy3;\n\
 \sptr_t *_dummy4;\n\
 \fptr_t *_dummy5;\n\
 \wptr_t *_dummy6;\n\
 \\n\
 \\n\
 \static int A_UNUSED\n\
 \lhc_valid_whnf(wptr_t s)\n\
 \{\n\
 \        return ((GETTAG(s) == P_VALUE) || ((GETTAG(s) == P_WHNF) && lhc_malloc_sanity(s,P_WHNF)));\n\
 \}\n\
 \\n\
 \static int A_UNUSED\n\
 \lhc_valid_lazy(sptr_t s)\n\
 \{\n\
 \        if(lhc_valid_whnf((wptr_t)s))\n\
 \                return 1;\n\
 \        assert(GETTAG(s) == P_LAZY);\n\
 \        node_t *ds = (sptr_t)DETAG(s);\n\
 \        assert(lhc_malloc_sanity(ds,P_LAZY));\n\
 \        if(ISLAZY(ds->head)) {\n\
 \                if(ds->head == BLACK_HOLE) return 1;\n\
 \                assert(GETTAG(ds->head) == P_FUNC);\n\
 \                fptr_t dhead = (fptr_t)DETAG(ds->head);\n\
 \                assert(dhead >= &_start && dhead < &_end);\n\
 \                return 1;\n\
 \        } else\n\
 \                return lhc_valid_whnf((wptr_t)ds->head);\n\
 \}\n\
 \\n\
 \\n\
 \#else\n\
 \\n\
 \#define lhc_valid_whnf(x) 1\n\
 \#define lhc_valid_lazy(x) 1\n\
 \\n\
 \#endif\n\
 \\n\
 \\n\
 \typedef wptr_t (*eval_fn)(node_t *node) A_STD;\n\
 \\n\
 \// both promote and demote evaluate to nothing when debugging is not enabled\n\
 \// otherwise, they check that their arguments are in the correct form.\n\
 \\n\
 \static inline wptr_t A_STD A_UNUSED  A_HOT\n\
 \promote(sptr_t s)\n\
 \{\n\
 \        assert(!ISLAZY(s));\n\
 \        assert(lhc_valid_whnf((wptr_t)s));\n\
 \        return (wptr_t)s;\n\
 \}\n\
 \\n\
 \static inline sptr_t A_STD A_UNUSED A_HOT\n\
 \demote(wptr_t s)\n\
 \{\n\
 \        assert(!ISLAZY(s));\n\
 \        assert(lhc_valid_whnf(s));\n\
 \        return (sptr_t)s;\n\
 \}\n\
 \\n\
 \// like eval but you know the target is in WHNF or is a already evaluated indirection\n\
 \static inline wptr_t A_STD A_UNUSED  A_HOT\n\
 \follow(sptr_t s)\n\
 \{\n\
 \        assert(lhc_valid_lazy(s));\n\
 \        if(ISLAZY(s)) {\n\
 \                sptr_t h = (sptr_t)(GETHEAD(DETAG(s)));\n\
 \                assert(!ISLAZY(h));\n\
 \                return (wptr_t)h;\n\
 \        }\n\
 \        return (wptr_t)s;\n\
 \}\n\
 \\n\
 \static inline wptr_t A_STD A_UNUSED  A_HOT\n\
 \eval(sptr_t s)\n\
 \{\n\
 \        assert(lhc_valid_lazy(s));\n\
 \        if(ISLAZY(s)) {\n\
 \                assert(GETTAG(s) == P_LAZY);\n\
 \                void *ds = (void *)DETAG(s);\n\
 \                sptr_t h = (sptr_t)(GETHEAD(ds));\n\
 \                assert(h != BLACK_HOLE);\n\
 \                if(ISLAZY(h)) {\n\
 \                        eval_fn fn = (eval_fn)DETAG(h);\n\
 \#if _LHC_DEBUG\n\
 \                        GETHEAD(ds) = BLACK_HOLE;\n\
 \#endif\n\
 \                        wptr_t r = (*fn)(NODEP(ds));\n\
 \#if _LHC_DEBUG\n\
 \                        assert(GETHEAD(ds) != BLACK_HOLE);\n\
 \#endif\n\
 \                        return r;\n\
 \                }\n\
 \                return (wptr_t)h;\n\
 \        }\n\
 \        assert(lhc_valid_whnf((wptr_t)s));\n\
 \        return (wptr_t)s;\n\
 \}\n\
 \\n\
 \\n\
 \static inline void A_STD A_UNUSED A_HOT\n\
 \update(sptr_t thunk, wptr_t new)\n\
 \{\n\
 \        lhc_update_inc();\n\
 \        assert(GETHEAD(thunk) == BLACK_HOLE);\n\
 \        assert(!ISLAZY(new));\n\
 \        GETHEAD(thunk) = (fptr_t)new;\n\
 \}\n\
 \\n\
 \\n\
 \\n\
 \\n\
 \"

-- | Generated from data\/ViaGhc.hs on Tue Nov 18 04:44:37 CET 2008
{-# NOINLINE viaghc_hs #-}
viaghc_hs :: String
viaghc_hs = "\
 \{-# OPTIONS_GHC -fglasgow-exts -fno-implicit-prelude #-}\n\
 \module Main(main) where\n\
 \\n\
 \import GHC.Int\n\
 \import GHC.Word\n\
 \import GHC.IOBase\n\
 \import GHC.Prim\n\
 \import GHC.Base\n\
 \import GHC.Ptr\n\
 \import GHC.Err\n\
 \\n\
 \type World__ = State# RealWorld\n\
 \type Array__ a = Array# a\n\
 \type MutArray__ a = MutableArray# RealWorld a\n\
 \type Ref__ a = MutVar# RealWorld a\n\
 \\n\
 \type Nothing = ()\n\
 \\n\
 \theNothing :: Nothing\n\
 \theNothing = ()\n\
 \\n\
 \type JIO a = World__ -> (# World__, a #)\n\
 \\n\
 \main :: IO ()\n\
 \main = IO $ \\rw -> case theRealMain rw of rw' -> (# rw', () #)\n\
 \\n\
 \unPtr :: Ptr a -> Addr#\n\
 \unPtr ptr = case ptr of\n\
 \    Ptr addr -> addr\n\
 \\n\
 \unFunPtr :: FunPtr a -> Addr#\n\
 \unFunPtr ptr = case ptr of\n\
 \    FunPtr addr -> addr\n\
 \\n\
 \fromBool :: Bool -> Int#\n\
 \fromBool b = case b of\n\
 \    False -> 0#\n\
 \    True -> 1#\n\
 \\n\
 \gteChar# a b = gtChar# a b || eqChar# a b\n\
 \lteChar# a b = ltChar# a b || eqChar# a b\n\
 \\n\
 \plusAddr__ :: Addr# -> Addr# -> Addr#\n\
 \plusAddr__ a1 a2 = plusAddr# a1 (addr2Int# a2)\n\
 \\n\
 \alloca__ :: Int# -> (Addr# -> JIO a) -> JIO a\n\
 \alloca__ size action s =\n\
 \     case newPinnedByteArray# size s      of { (# s, mbarr# #) ->\n\
 \     case unsafeFreezeByteArray# mbarr# s of { (# s, barr#  #) ->\n\
 \     case action (byteArrayContents# barr#) s of { (# s, r #) ->\n\
 \     case touch# barr# s of { s -> (# s, r #) }\n\
 \     }}}\n\
 \\n\
 \word2Char__ x = chr# (word2Int# x)\n\
 \char2Word__ x = int2Word# (ord# x)\n\
 \addr2Word__ x = int2Word# (addr2Int# x)\n\
 \word2Addr__ x = int2Addr# (word2Int# x)\n\
 \\n\
 \convertString :: [Char] -> ListTCon Char\n\
 \convertString [] = lhc_EmptyList\n\
 \convertString (x:xs) = lhc_Cons x (convertString xs)\n\
 \\n\
 \{-\n\
 \error__ :: Addr# -> a\n\
 \error__ s = unsafePerformIO $ do\n\
 \    error_show s\n\
 \    error_exit (I# 255#)\n\
 \\n\
 \errorInt__ :: Addr# -> Int#\n\
 \errorInt__ s = seq (unsafePerformIO $ do\n\
 \    error_show s\n\
 \    error_exit (I# 255#)) 0#\n\
 \\n\
 \errorWord__ :: Addr# -> Word#\n\
 \errorWord__ s = seq (unsafePerformIO $ do\n\
 \    error_show s\n\
 \    error_exit (I# 255#)) (int2Word# 0#)\n\
 \\n\
 \errorAddr__ :: Addr# -> Addr#\n\
 \errorAddr__ s = seq (unsafePerformIO $ do\n\
 \    error_show s\n\
 \    error_exit (I# 255#)) (int2Addr# 0#)\n\
 \foreign import ccall unsafe \"puts\" error_show :: Ptr a -> IO ()\n\
 \foreign import ccall unsafe \"exit\" error_exit :: Int -> IO a\n\
 \ -}\n\
 \\n\
 \{-# NOINLINE newWorld__ #-}\n\
 \newWorld__ :: a -> World__\n\
 \newWorld__ a = case lazy a of\n\
 \    _ -> realWorld#\n\
 \\n\
 \theRealMain :: World__ -> World__\n\
 \\n\
 \"

-- | Generated from data\/prelude.m4 on Tue Nov 18 04:44:37 CET 2008
{-# NOINLINE prelude_m4 #-}
prelude_m4 :: String
prelude_m4 = "\
 \m4_changequote({{,}})\n\
 \m4_changecom({-,-})\n\
 \\n\
 \m4_define(ONCE,{{m4_ifdef(done-$1,{{m4_dnl}},{{m4_define(done-$1,1)$1}})}})\n\
 \"
