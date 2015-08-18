#ifndef INCLUDED_CALC_CMDLINECALC
#define INCLUDED_CALC_CMDLINECALC

#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif
#ifndef INCLUDED_PCRDLL
#include "pcrdll.h"
#define INCLUDED_PCRDLL
#endif

namespace calc {

//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------
extern "C" PCR_DLL_FUNC(int) executeCommandLine(int   argc, char**argv);

} // namespace calc

#endif
