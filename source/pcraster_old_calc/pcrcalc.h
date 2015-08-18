#ifndef INCLUDED_PCR_PCRCALC
#define INCLUDED_PCR_PCRCALC

#ifndef INCLUDED_PCRDLL
#include "pcrdll.h"
#define INCLUDED_PCRDLL
#endif

/*
 * All rights reserverd bla bla bla bla
 * you should have a license bla bla bla
 * bla bla bla bla bla bla bla bla bla bla
 */

typedef struct PcrScriptImpl PcrScript;

#ifdef __cplusplus
 extern "C" {
#endif


PCR_DLL_FUNC(PcrScript* ) pcr_createScript(const char* scriptName);
PCR_DLL_FUNC(void       ) pcr_ScriptExecute(PcrScript *script);
PCR_DLL_FUNC(int        ) pcr_ScriptError(PcrScript *script);
PCR_DLL_FUNC(const char*) pcr_ScriptErrorMessage(PcrScript *script);
PCR_DLL_FUNC(void       ) pcr_destroyScript(PcrScript *script);

#ifdef __cplusplus
 }
#endif

#endif
