
static void _amain(void);
static void lhc_arch_assert(void);
static int lhc_argc;
static char **lhc_argv;
static char *lhc_progname;
static jmp_buf lhc_uncaught;

static HsInt lhc_stdrnd[2] A_UNUSED = { 1 , 1 };

#if _LHC_PROFILE

static uintmax_t lhc_prof_function_calls;
static uintmax_t lhc_prof_case_statements;
static uintmax_t lhc_prof_updates;

#define lhc_update_inc()   lhc_prof_updates++
#define lhc_function_inc() lhc_prof_function_calls++
#define lhc_case_inc()     lhc_prof_case_statements++

#else

#define lhc_update_inc()    do { } while(0)
#define lhc_function_inc()  do { } while(0)
#define lhc_case_inc()      do { } while(0)

#endif

static void A_COLD
lhc_print_profile(void) {
        struct tms tm;
        times(&tm);
        if(!(_LHC_PROFILE || getenv("LHC_RTS_PROFILE"))) return;

        fprintf(stderr, "\n-----------------\n");
        fprintf(stderr, "Profiling: %s\n", lhc_progname);
        fprintf(stderr, "Command: %s\n", lhc_command);
        fprintf(stderr, "Complie: %s\n", lhc_c_compile);
        fprintf(stderr, "Version: %s\n\n", lhc_version);
        lhc_alloc_print_stats();
        float cpt = (float)sysconf(_SC_CLK_TCK);
        fprintf(stderr, "User Time:   %.2fs\n", (float)tm.tms_utime/cpt);
        fprintf(stderr, "System Time: %.2fs\n", (float)tm.tms_stime/cpt);
        fprintf(stderr, "Total Time:  %.2fs\n", (float)(tm.tms_stime + tm.tms_utime)/cpt);

#if _LHC_PROFILE
        fprintf(stderr, "\nFunction Calls:   %llu\n", (unsigned long long)lhc_prof_function_calls);
        fprintf(stderr, "Case Statements:  %llu\n", (unsigned long long)lhc_prof_case_statements);
        fprintf(stderr, "Updates:          %llu\n", (unsigned long long)lhc_prof_updates);
#endif
        fprintf(stderr, "-----------------\n");
}


static void A_NORETURN A_UNUSED A_COLD
lhc_exit(int n) {
        lhc_print_profile();
        exit(n);
}

static void  A_NORETURN A_UNUSED  A_COLD
lhc_error(char *s) {
        fputs(s,stderr);
        fputs("\n",stderr);
        lhc_print_profile();
        exit(255);
}

static void  A_NORETURN A_UNUSED  A_COLD
lhc_case_fell_off(int n) {
        fflush(stdout);
        fprintf(stderr, "\n%s:%i: case fell off\n", __FILE__, n);
        abort();
}

#define lhc_setjmp(jb) sigsetjmp(*(jmp_buf *)jb,0)
#define lhc_longjmp(jb) siglongjmp(*(jmp_buf *)jb,1)

struct lhc_continuation {
    void *argument;
    jmp_buf jump_buf;
};

#define prim_umaxbound(t) ((t)~((t)0))
#define prim_maxbound(t) ((t)(~((t)1 << (sizeof(t)*8 - 1))))
#define prim_minbound(t) ((t)(((t)1 << (sizeof(t)*8 - 1))))


inline static int A_UNUSED
lhc_utf8_getchar(void)
{
    return getchar_unlocked();
}

inline static int A_UNUSED
lhc_utf8_getc(FILE *f)
{
    return getc_unlocked(f);
}

inline static int A_UNUSED
lhc_utf8_putchar(int ch)
{
    return putchar_unlocked(ch);
}

inline static int A_UNUSED
lhc_utf8_putc(int ch, FILE *f)
{
    return putc_unlocked(ch,f);
}


int  A_COLD
main(int argc, char *argv[])
{
        /* A few random assertions about the architecture that the compiler
         * assumes. should be true of any but the oddest of beasts.
         */

        assert(sizeof(HsPtr) == sizeof(HsFunPtr));
        assert(sizeof(HsPtr) == sizeof(intptr_t));
        assert(sizeof(HsPtr) == sizeof(uintptr_t));
        assert(CHAR_BIT == 8);
        assert(EOF == -1);

        lhc_arch_assert();
        lhc_malloc_init();
        lhc_argc = argc - 1;
        lhc_argv = argv + 1;
        lhc_progname = argv[0];
        setlocale(LC_ALL,"");
        if (sigsetjmp(lhc_uncaught,0))
                lhc_error("Uncaught Exception");
        else
                _amain();
        lhc_print_profile();
        return 0;
}




