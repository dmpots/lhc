#ifndef __GUARD_RTSGHC_H
#define __GUARD_RTSGHC_H
#include "HsFFI.h"
#include <stdarg.h>

StgStablePtr getOrSetTypeableStore(StgStablePtr ptr);
StgStablePtr getOrSetGHCConcSignalHandlerStore(StgStablePtr ptr);
StgStablePtr getOrSetGHCConcPendingEventsStore(StgStablePtr ptr);
StgStablePtr getOrSetGHCConcPendingDelaysStore(StgStablePtr ptr);
StgStablePtr getOrSetGHCConcIOManagerThreadStore(StgStablePtr ptr);
StgStablePtr getOrSetGHCConcProddingStore(StgStablePtr ptr);
void barf(const char*s, ...);
void errorBelch(const char*s, ...);
void verrorBelch(const char*s, va_list ap);
void rtsFatalInternalErrorFn(const char *s, va_list ap);
void rtsErrorMsgFn(const char *s, va_list ap);
void rtsDebugMsgFn(const char *s, va_list ap);
HsBool rtsSupportsBoundThreads(void);

void hs_free_stable_ptr(HsStablePtr sp);
#endif
