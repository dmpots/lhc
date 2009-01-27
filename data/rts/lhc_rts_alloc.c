
// some default definitions

#define lhc_malloc_whnf lhc_malloc
#define lhc_malloc_suspension lhc_malloc
#define lhc_malloc_atomic lhc_malloc
#define lhc_malloc_atomic_whnf lhc_malloc_atomic
#define lhc_malloc_sanity(p,t) (1)

#if _LHC_PROFILE

#define BUCKETS 7

static unsigned alloced[BUCKETS];
static unsigned alloced_atomic[BUCKETS];

static void
alloc_count(int n,int atomic)
{
        n = n ? ((n - 1)/sizeof(void *)) + 1 : 0;
        n = n > BUCKETS - 1 ? BUCKETS - 1 : n;
        (atomic ? alloced_atomic : alloced)[n]++;
}


static void
print_alloc_size_stats(void) {
        char fmt[] = "%10s %10s %10s %10s %10s\n";
        char fmt2[] = "%10u %10u %10u %10u %10u\n";
        fprintf(stderr,fmt,"Size","Normal","Atomic","Total","Accum");
        fprintf(stderr,fmt,"----","------","------","-----","-----");
        unsigned accum = 0;
        for(int i = 0; i < BUCKETS; i++) {
                accum += alloced[i] + alloced_atomic[i];
                fprintf(stderr,fmt2,i,alloced[i],alloced_atomic[i],alloced_atomic[i] + alloced[i], accum);
        }
}


#else

#define alloc_count(x,y)
#define print_alloc_size_stats()

#endif /* _LHC_PROFILE */

#if _LHC_GC == _LHC_GC_BOEHM

#include <gc/gc.h>

#define lhc_malloc GC_malloc
#undef  lhc_malloc_atomic
#define lhc_malloc_atomic GC_malloc_atomic
#define lhc_free GC_free

static inline void lhc_malloc_init(void) { GC_INIT(); }
static inline void lhc_alloc_print_stats(void) { GC_dump(); }

#elif _LHC_GC == _LHC_GC_NONE

// memory allocated in 1MB chunks.
#define LHC_MEM_CHUNK_SIZE (1 << 20)

static char initial_chunk[LHC_MEM_CHUNK_SIZE];

static void *lhc_current_chunk = initial_chunk;
static unsigned mem_chunks,mem_offset;


static inline void
lhc_malloc_init(void) { return; }

static void
lhc_alloc_print_stats(void) {
        fprintf(stderr, "Memory Allocated: %u bytes\n", (LHC_MEM_CHUNK_SIZE*(mem_chunks)) + mem_offset);
        print_alloc_size_stats();
}

static void
lhc_malloc_grow(void) {
        void *c = malloc(LHC_MEM_CHUNK_SIZE);
        if(!c) {
                fputs("Out of memory!\n",stderr);
                abort();
        }
        mem_chunks++;
        lhc_current_chunk = c;
        mem_offset = 0;
}

static inline void * A_MALLOC
lhc_malloc_basic(size_t n) {
        n = ALIGN(sizeof(void *),n);
        if (n > (LHC_MEM_CHUNK_SIZE - mem_offset))
                lhc_malloc_grow();
        void *ret = lhc_current_chunk + mem_offset;
        mem_offset += n;
        return ret;
}


#if _LHC_DEBUG

#define lhc_malloc(n) lhc_malloc_debug(n,__LINE__,0)
#undef lhc_malloc_atomic
#define lhc_malloc_atomic(n) lhc_malloc_debug(n,__LINE__,1)

static void * A_MALLOC
lhc_malloc_debug(size_t n,int line,int atomic) {
        alloc_count(n,atomic);
        void *ret = lhc_malloc_basic(n + sizeof(uintptr_t));
        *((uintptr_t *)ret) = line;
        return ret + sizeof(uintptr_t);
}

#else

static inline void * A_MALLOC
lhc_malloc(size_t n) {
        alloc_count(n,0);
        return lhc_malloc_basic(n);
}

#undef lhc_malloc_atomic
static inline void * A_MALLOC
lhc_malloc_atomic(size_t n) {
        alloc_count(n,1);
        return lhc_malloc_basic(n);
}


#endif /* _LHC_DEBUG */

#endif /* _LHC_GC == _LHC_GC_BOEHM */




