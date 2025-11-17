#ifndef INCLUDED_PCRXML_OSTREAM
#define INCLUDED_PCRXML_OSTREAM

#include "stddefx.h"

#include <iostream>


class QDomDocument;

std::ostream&      operator<<          (std::ostream& s,
                                        const QDomDocument& d);
/*
class QString;
std::ostream&      operator<<          (std::ostream& s,
                                        const QString& str);
std::ostream&      operator<<          (std::ostream& target,
                                        const XMLCh*     s);
 */
#endif
