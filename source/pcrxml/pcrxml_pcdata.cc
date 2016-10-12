#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_PCRXML_PCDATA
#include "pcrxml_pcdata.h"
#endif

#ifndef INCLUDED_COM_STRLIB
#include "com_strlib.h"
#define INCLUDED_COM_STRLIB
#endif

#ifndef  INCLUDED_QDOM
#include <qdom.h>
#define  INCLUDED_QDOM
#endif


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
      QDomCharacterData c(node.toCharacterData());
      QByteArray asciiData = c.data().toLatin1();
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
