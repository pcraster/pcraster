#ifndef INCLUDED_PCRXML_STRINGCONV
#define INCLUDED_PCRXML_STRINGCONV

#include "stddefx.h"

#include <string>

class QString;

namespace pcrxml {

std::string asString(const QString& s);
// std::string asString(const XMLCh *s);
QString asQString(const std::string& s);

} // namespace pcrxml


#endif
