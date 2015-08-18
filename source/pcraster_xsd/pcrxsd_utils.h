#ifndef INCLUDED_PCRXSD_UTILS
#define INCLUDED_PCRXSD_UTILS



// #ifndef INCLUDED_STDDEFX
// #include "stddefx.h"
// #define INCLUDED_STDDEFX
// #endif

// Library headers.
#include <xercesc/util/XercesDefs.hpp>

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_IOSTREAM
#include <iostream>
#define INCLUDED_IOSTREAM
#endif
#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif


namespace pcrxsd {
  // utils declarations.
}



namespace pcrxsd {


//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------

std::string contentsIsXMLOrPCRasterFileFormat(std::string const& contents);
std::string toString(const XMLCh* const toTranscode);

} // namespace pcrxsd

inline std::ostream& operator<<(std::ostream& target, const XMLCh * toDump)
{
    target << pcrxsd::toString(toDump);
    return target;
}

#endif
