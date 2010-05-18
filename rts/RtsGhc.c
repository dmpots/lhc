/* Fakeing some Ghc RTS calls */
#include <stdio.h>
#include <stdlib.h>
#include "ghc/RtsGhc.h"


typedef void RtsMsgFunction(const char *, va_list);
RtsMsgFunction *fatalInternalErrorFn = rtsFatalInternalErrorFn;
RtsMsgFunction *errorMsgFn = rtsErrorMsgFn;
RtsMsgFunction *debugMsgFn = rtsDebugMsgFn;

StgStablePtr
getOrSetTypeableStore(StgStablePtr ptr)
{
    errorBelch("getOrSetTypeableStore -- NOT IMPLEMENTED");
    return NULL;
}

StgStablePtr
getOrSetGHCConcSignalHandlerStore(StgStablePtr ptr)
{
    errorBelch("getOrSetGHCConcSignalHandlerStore -- NOT IMPLEMENTED");
    return NULL;
}

StgStablePtr
getOrSetGHCConcPendingEventsStore(StgStablePtr ptr)
{
    errorBelch("getOrSetGHCConcPendingEventsStore -- NOT IMPLEMENTED");
    return NULL;
}

StgStablePtr
getOrSetGHCConcPendingDelaysStore(StgStablePtr ptr)
{
    errorBelch("getOrSetGHCConcPendingDelaysStore -- NOT IMPLEMENTED");
    return NULL;
}

StgStablePtr
getOrSetGHCConcIOManagerThreadStore(StgStablePtr ptr)
{
    errorBelch("getOrSetGHCConcIOManagerThreadStore -- NOT IMPLEMENTED");
    return NULL;
}

StgStablePtr
getOrSetGHCConcProddingStore(StgStablePtr ptr)
{
    errorBelch("getOrSetGHCConcProddingStore -- NOT IMPLEMENTED");
    return NULL;
}

void
barf(const char*s, ...)
{
  va_list ap;
  va_start(ap,s);
  (*fatalInternalErrorFn)(s,ap);
  abort();
  va_end(ap);
}

void
errorBelch(const char*s, ...)
{
  va_list ap;
  va_start(ap,s);
  (*errorMsgFn)(s,ap);
  va_end(ap);
}

void
verrorBelch(const char*s, va_list ap)
{
  (*errorMsgFn)(s,ap);
}

void
debugBelch(const char*s, ...)
{
  va_list ap;
  va_start(ap,s);
  (*debugMsgFn)(s,ap);
  va_end(ap);
}

void
rtsErrorMsgFn(const char *s, va_list ap)
{
  /* don't fflush(stdout); WORKAROUND bug in Linux glibc */
  fprintf(stderr, "%s: ", "lhc");
  vfprintf(stderr, s, ap);
  fprintf(stderr, "\n");
}

void
rtsDebugMsgFn(const char *s, va_list ap)
{
    /* don't fflush(stdout); WORKAROUND bug in Linux glibc */
    vfprintf(stderr, s, ap);
    fflush(stderr);
}

void 
rtsFatalInternalErrorFn(const char *s, va_list ap)
{
  /* don't fflush(stdout); WORKAROUND bug in Linux glibc */
  fprintf(stderr, "lhc: internal error: ");
  vfprintf(stderr, s, ap);
  fprintf(stderr, "\n");
  fprintf(stderr, "    Please report this as a LHC bug\n");
  fflush(stderr);

  abort();
}


HsBool
rtsSupportsBoundThreads(void)
{
  return HS_BOOL_FALSE;
}

/* -----------------------------------------------------------------------------
 * Install a Haskell signal handler.
 *
 * We should really do this in Haskell in GHC.Conc, and share the
 * signal_handlers array with the one there.
 *
 * -------------------------------------------------------------------------- */

int
stg_sig_install(int sig, int spi, void *mask)
{
    return 0;
    /*
    sigset_t signals, osignals;
    struct sigaction action;
    StgInt previous_spi;

    // Block the signal until we figure out what to do
    // Count on this to fail if the signal number is invalid
    if (sig < 0 || sigemptyset(&signals) ||
	sigaddset(&signals, sig) || sigprocmask(SIG_BLOCK, &signals, &osignals)) {
	return STG_SIG_ERR;
    }
    
    more_handlers(sig);

    previous_spi = signal_handlers[sig];

    action.sa_flags = 0;
    
    switch(spi) {
    case STG_SIG_IGN:
        action.sa_handler = SIG_IGN;
    	break;

    case STG_SIG_DFL:
        action.sa_handler = SIG_DFL;
    	break;

    case STG_SIG_RST:
        action.sa_flags |= SA_RESETHAND;
        // fall through 
    case STG_SIG_HAN:
    	action.sa_sigaction = generic_handler;
        action.sa_flags |= SA_SIGINFO;
    	break;

    default:
        barf("stg_sig_install: bad spi");
    }

    if (mask != NULL)
        action.sa_mask = *(sigset_t *)mask;
    else
	sigemptyset(&action.sa_mask);

    action.sa_flags |= sig == SIGCHLD && nocldstop ? SA_NOCLDSTOP : 0;

    if (sigaction(sig, &action, NULL))
    {
        errorBelch("sigaction");
	return STG_SIG_ERR;
    }

    signal_handlers[sig] = spi;

    switch(spi) {
    case STG_SIG_RST:
    case STG_SIG_HAN:
	sigaddset(&userSignals, sig);
        if (previous_spi != STG_SIG_HAN && previous_spi != STG_SIG_RST) {
            n_haskell_handlers++;
        }
    	break;

    default:
	sigdelset(&userSignals, sig);
        if (previous_spi == STG_SIG_HAN || previous_spi == STG_SIG_RST) {
            n_haskell_handlers--;
        }
        break;
    }

    if (sigprocmask(SIG_SETMASK, &osignals, NULL))
    {
        errorBelch("sigprocmask");
	return STG_SIG_ERR;
    }

    return previous_spi;
  */
}

void hs_free_stable_ptr(HsStablePtr sp)
{
  errorBelch("%s: hs_free_stable_ptr not implemented", __FILE__);
}
