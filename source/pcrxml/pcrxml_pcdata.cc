#include "stddefx.h"
#include "pcrxml_pcdata.h"
#include "com_strlib.h"

#include <QDomDocument>

/*!
  \file
  This file contains the implementation of the (#PCDATA) element
  convertors to types such as int, size_t and strings
*/

//! convert contents of \a n which is of type (#PCDATA)
size_t pcrxml::pcdataToSize_t(const QDomNode& n)
{
  return com::strToSize_t(pcdataToString(n));
}

//! convert contents of \a n which is of type (#PCDATA)
int pcrxml::pcdataToInt(const QDomNode& n)
{
  return com::strToInt(pcdataToString(n));
}

//! convert contents of \a n which is of type (#PCDATA)
std::string pcrxml::pcdataToString(const QDomNode& n)
{
  PRECOND(!n.isNull());
  PRECOND(!n.firstChild().isNull());

  std::string s;
  QDomNode node = n.firstChild();
  while(!node.isNull()) {
    if(node.nodeType() == QDomNode::TextNode) {
      PRECOND(node.isCharacterData());
      QDomCharacterData const c(node.toCharacterData());
      QByteArray const asciiData = c.data().toLatin1();
      s += asciiData.constData();
    }
    node = node.nextSibling();
  }

  PRECOND(!s.empty());

#ifdef DEBUG_DEVELOP
  if(!node.isNull()) {
    node = node.nextSibling();
    while(!node.isNull()) {
      PRECOND(node.nodeType() != QDomNode::TextNode);
      node = node.nextSibling();
    }
  }
#endif

  return s;
}
