#ifndef INCLUDED_CALC_CMDLINECALC
#define INCLUDED_CALC_CMDLINECALC

#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#include "pcraster_model_engine_export.h"

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
extern "C" PCR_ME_EXPORT int executeCommandLine(int   argc, char**argv);

} // namespace calc

#endif
