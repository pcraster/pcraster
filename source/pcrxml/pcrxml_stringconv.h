#ifndef INCLUDED_PCRXML_STRINGCONV
#define INCLUDED_PCRXML_STRINGCONV


#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_STRING
# include <string>
# define INCLUDED_STRING
#endif

class QString;

namespace pcrxml {

std::string asString(const QString& s);
// std::string asString(const XMLCh *s);
QString asQString(const std::string& s);

} // namespace pcrxml


#endif
