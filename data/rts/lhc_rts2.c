
#define ISLAZY(x)    (((uintptr_t)(x)) & 0x1)
#define DETAG(x)     ((uintptr_t)(x) & ~0x3)
#define GETTAG(x)    ((uintptr_t)(x) & 0x3)

#define GETHEAD(x)   (NODEP(x)->head)
#define NODEP(x)     ((node_t *)(x))
#define DNODEP(x)    ((dnode_t *)(x))
#define EVALTAG(fn)  (assert(((uintptr_t)(fn) & 0x3) == 0),(sptr_t)((uintptr_t)(fn) | P_LAZY))
#define EVALTAGC(fn) ((sptr_t)((void *)(fn) + P_LAZY))
#define EVALFUNC(fn) ((fptr_t)((uintptr_t)(fn) | P_FUNC))
#define VALUE(n)     ((wptr_t)(((intptr_t)(n) << 2) | P_VALUE))
#define GETVALUE(n)  ((intptr_t)(n) >> 2)
#define ISVALUE(n)   (assert(!ISLAZY(n)), ((uintptr_t)(n) & 0x2))
#define PROMOTE(n)   ((wptr_t)(n))
#define DEMOTE(n)    ((sptr_t)(n))
#define GETWHAT(x)   (GETTAG(x) == P_VALUE ? ((uintptr_t)(x) >> 16) : DNODEP(x)->what)

#define SETWHAT(x,v)   (DNODEP(x)->what = (v))
#define RAWWHAT(w)     (wptr_t)(((uintptr_t)w << 16) | P_VALUE)


#define P_WHNF  0x0
#define P_LAZY  0x1
#define P_VALUE 0x2
#define P_FUNC  0x3

#define BLACK_HOLE ((fptr_t)0xDEADBEEF)


/*@Internals

# The Run Time System

Lhc is very minimalist in that it does not have a precompiled run time system,
but rather generates what is needed as part of the compilation process.
However, we call whatever conventions and binary layouts used in the generated
executable the run time system. Since lhc generates the code anew each time, it
can build a different 'run time' based on compiler options, trading things like
the garbage collector as needed or changing the closure layout when we know we
have done full program optimization. This describes the 'native' layout upon
which other conventions are layered.

A basic value in lhc is represented by a 'smart pointer' of c type sptr_t. a
smart pointer is the size of a native pointer, but can take on different roles
depending on a pair of tag bits.

smart pointers take on a general form as follows:

    -------------------------
    |    payload        | GL|
    -------------------------

      G - if set, then the garbage collector should not treat value as a pointer to be followed
      L - lazy, this bit being set means the value is not in WHNF

A raw sptr_t on its own in the wild can only take on one of the following values:

    -------------------------
    |    raw value      | 10|
    -------------------------

    -------------------------
    |    whnf location  | 00|
    -------------------------

    -------------------------
    |   lazy location   | 01|
    -------------------------

A raw value can be anything and not necessarily a pointer in general, a WHNF
location is a pointer to some value in WHNF. The system places no restrictions
on what is actually pointed to by a WHNF pointer, however the garbage collector
in use may. In general, the back end is free to choose what to place in the raw
value field or in what a WHNF points to with complete freedom. If an
implementation sees the L bit is clear, it can pass on the smart pointer
without examining it knowing the value is in WHNF.

A lazy location points to a potential closure or an indirection to a WHNF
value. The lazy location is an allocated chunk of memory that is at least
one pointer long. the very first location in a closure must be one of the
following.

    -------------------------
    | raw value or whnf  |X0|
    -------------------------

An evaluated value, interpreted exactly as above. one can always replace any occurance of a
lazy location with an evaluated indirecton.

    -------------------------
    |    code pointer   | 11|
    -------------------------
    |     data ...          |

This is something to evaluate, code pointer is a pointer to a function that takes
the memory location as its only argument, the called function is in charge
of updating the location if needed.

note that it is invalid to have a lazy location point to another lazy
location. there is only ever one level of indirection allowed, and only from
lazy locations

note that a partial application is just like any other value in WHNF as far
as the above is concered. It happens to possibly contain a code pointer.

*/


/*
 * type names
 *
 * sptr_t - a tagged smart pointer, may be a value, may be a pointer to a whnf or lazy location
 * wptr_t - a value guarenteed to be in whnf
 * fptr_t - a pointer to a whnf or a function pointer to something to evaluate, first value in a lazy location.
 * what_t  - the discriminator of a discriminated union
 *
 */

typedef struct node *  sptr_t;
typedef struct dnode * wptr_t;
typedef void *         fptr_t;
typedef uintptr_t      what_t;


typedef struct node {
        fptr_t head;
        sptr_t rest[];
} A_MAYALIAS node_t;

typedef struct dnode {
        what_t what;
        sptr_t rest[];
} A_MAYALIAS dnode_t;

#if _LHC_DEBUG

// these ensure the type synonyms are available to the debugger
uintptr_t _dummy1;
node_t *_dummy2;
dnode_t *_dummy3;
sptr_t *_dummy4;
fptr_t *_dummy5;
wptr_t *_dummy6;


static int A_UNUSED
lhc_valid_whnf(wptr_t s)
{
        return ((GETTAG(s) == P_VALUE) || ((GETTAG(s) == P_WHNF) && lhc_malloc_sanity(s,P_WHNF)));
}

static int A_UNUSED
lhc_valid_lazy(sptr_t s)
{
        if(lhc_valid_whnf((wptr_t)s))
                return 1;
        assert(GETTAG(s) == P_LAZY);
        node_t *ds = (sptr_t)DETAG(s);
        assert(lhc_malloc_sanity(ds,P_LAZY));
        if(ISLAZY(ds->head)) {
                if(ds->head == BLACK_HOLE) return 1;
                assert(GETTAG(ds->head) == P_FUNC);
                fptr_t dhead = (fptr_t)DETAG(ds->head);
                assert(dhead >= &_start && dhead < &_end);
                return 1;
        } else
                return lhc_valid_whnf((wptr_t)ds->head);
}


#else

#define lhc_valid_whnf(x) 1
#define lhc_valid_lazy(x) 1

#endif


typedef wptr_t (*eval_fn)(node_t *node) A_STD;

// both promote and demote evaluate to nothing when debugging is not enabled
// otherwise, they check that their arguments are in the correct form.

static inline wptr_t A_STD A_UNUSED  A_HOT
promote(sptr_t s)
{
        assert(!ISLAZY(s));
        assert(lhc_valid_whnf((wptr_t)s));
        return (wptr_t)s;
}

static inline sptr_t A_STD A_UNUSED A_HOT
demote(wptr_t s)
{
        assert(!ISLAZY(s));
        assert(lhc_valid_whnf(s));
        return (sptr_t)s;
}

// like eval but you know the target is in WHNF or is a already evaluated indirection
static inline wptr_t A_STD A_UNUSED  A_HOT
follow(sptr_t s)
{
        assert(lhc_valid_lazy(s));
        if(ISLAZY(s)) {
                sptr_t h = (sptr_t)(GETHEAD(DETAG(s)));
                assert(!ISLAZY(h));
                return (wptr_t)h;
        }
        return (wptr_t)s;
}

static inline wptr_t A_STD A_UNUSED  A_HOT
eval(sptr_t s)
{
        assert(lhc_valid_lazy(s));
        if(ISLAZY(s)) {
                assert(GETTAG(s) == P_LAZY);
                void *ds = (void *)DETAG(s);
                sptr_t h = (sptr_t)(GETHEAD(ds));
                assert(h != BLACK_HOLE);
                if(ISLAZY(h)) {
                        eval_fn fn = (eval_fn)DETAG(h);
#if _LHC_DEBUG
                        GETHEAD(ds) = BLACK_HOLE;
#endif
                        wptr_t r = (*fn)(NODEP(ds));
#if _LHC_DEBUG
                        assert(GETHEAD(ds) != BLACK_HOLE);
#endif
                        return r;
                }
                return (wptr_t)h;
        }
        assert(lhc_valid_whnf((wptr_t)s));
        return (wptr_t)s;
}


static inline void A_STD A_UNUSED A_HOT
update(sptr_t thunk, wptr_t new)
{
        lhc_update_inc();
        assert(GETHEAD(thunk) == BLACK_HOLE);
        assert(!ISLAZY(new));
        GETHEAD(thunk) = (fptr_t)new;
}



