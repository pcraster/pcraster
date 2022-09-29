#ifndef INCLUDED_CALC_GLOBALLIBDEFS
#define INCLUDED_CALC_GLOBALLIBDEFS

#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.
#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif


// PCRaster library headers.
#include "pcraster_model_engine_export.h"

// Module headers.


namespace calc {

//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------

void         libError      (const std::string& msg);
void         throwLibError ();
std::string  getLibError   ();

PCR_ME_EXPORT void globalInit    ();
PCR_ME_EXPORT void setRan  (size_t seed);
PCR_ME_EXPORT int  parseGlobalFlag(std::string const& option);
PCR_ME_EXPORT void globalEnd     ();

} // namespace calc

#endif
