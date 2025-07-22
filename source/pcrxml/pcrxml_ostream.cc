#include "stddefx.h"
#include "pcrxml_ostream.h"
#include "pcrxml_stringconv.h"

#include <QDomDocument>
#include <QTextStream>

#include <iostream>


std::ostream& operator<< (std::ostream& s,
                          const QDomDocument& d)
{
  QString   str;
  QTextStream qs(&str);
  d.save(qs,1);
  s << pcrxml::asString(str);
  return s;
}
