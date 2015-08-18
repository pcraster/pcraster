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
#ifndef INCLUDED_PCRDLL
#include "pcrdll.h"
#define INCLUDED_PCRDLL
#endif

// Module headers.


namespace calc {

//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------

void         libError      (const std::string& msg);
void         throwLibError (void);
std::string  getLibError   (void);

PCR_DLL_FUNC(void) globalInit    ();
PCR_DLL_FUNC(void) setRan  (size_t seed);
PCR_DLL_FUNC(int)  parseGlobalFlag(std::string const& option);
void         globalEnd     ();

} // namespace calc

#endif
