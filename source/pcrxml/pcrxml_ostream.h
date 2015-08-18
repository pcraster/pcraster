#ifndef INCLUDED_PCRXML_OSTREAM
#define INCLUDED_PCRXML_OSTREAM


#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------

#ifndef INCLUDED_IOSTREAM
#include <iostream>
#define INCLUDED_IOSTREAM
#endif


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
