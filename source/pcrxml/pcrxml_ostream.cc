#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_PCRXML_OSTREAM
#include "pcrxml_ostream.h"
#define INCLUDED_PCRXML_OSTREAM
#endif

#ifndef INCLUDED_QTEXTSTREAM
#include <qtextstream.h>
#define INCLUDED_QTEXTSTREAM
#endif


#ifndef INCLUDED_IOSTREAM
#include <iostream>
#define INCLUDED_IOSTREAM
#endif

#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif

#ifndef INCLUDED_PCRXML_STRINGCONV
#include "pcrxml_stringconv.h"
#define INCLUDED_PCRXML_STRINGCONV
#endif

#ifndef INCLUDED_QSTRING
#include <qstring.h>
#define INCLUDED_QSTRING
#endif

#ifndef INCLUDED_QDOM
#include <qdom.h>
#define INCLUDED_QDOM
#endif

std::ostream& operator<< (std::ostream& s,
                          const QDomDocument& d)
{
  QString   str;
  QTextStream qs(&str);
  d.save(qs,1);
  s << pcrxml::asString(str);
  return s;
}
