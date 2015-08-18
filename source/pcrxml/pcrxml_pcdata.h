#ifndef INCLUDED_PCRXML_PCDATA
#define INCLUDED_PCRXML_PCDATA



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif


class QDomNode;

namespace pcrxml {

size_t             pcdataToSize_t       (const QDomNode& n);
int                pcdataToInt          (const QDomNode& n);
std::string        pcdataToString       (const QDomNode& n);

} // namespace pcrxml

#endif
