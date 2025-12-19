#ifndef INCLUDED_CALC_GLOBALLIBDEFS
#define INCLUDED_CALC_GLOBALLIBDEFS

#include "stddefx.h"
#include "pcraster_model_engine_export.h"

#include <string>



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
