#ifndef INCLUDED_PCRXML_PCDATA
#define INCLUDED_PCRXML_PCDATA

#include "stddefx.h"

#include <string>


class QDomNode;

namespace pcrxml {

size_t             pcdataToSize_t       (const QDomNode& n);
int                pcdataToInt          (const QDomNode& n);
std::string        pcdataToString       (const QDomNode& n);

} // namespace pcrxml

#endif
