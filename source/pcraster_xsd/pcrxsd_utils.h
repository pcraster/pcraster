#ifndef INCLUDED_PCRXSD_UTILS
#define INCLUDED_PCRXSD_UTILS

#include <xercesc/util/XercesDefs.hpp>

#include <iostream>
#include <string>



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
