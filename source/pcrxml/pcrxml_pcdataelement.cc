#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_PCRXML_PCDATAELEMENT
#include "pcrxml_pcdataelement.h"
#define INCLUDED_PCRXML_PCDATAELEMENT
#endif

// Standard library headers.

// Library headers.
#ifndef INCLUDED_QDOM
#include <qdom.h>
#define INCLUDED_QDOM
#endif

#ifndef INCLUDED_COM_EXCEPTION
#include "com_exception.h"
#define INCLUDED_COM_EXCEPTION
#endif

#ifndef INCLUDED_PCRXML_STRINGCONV
#include "pcrxml_stringconv.h"
#define INCLUDED_PCRXML_STRINGCONV
#endif


// Application headers.




//------------------------------------------------------------------------------
// DEFINITION OF STATIC MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF MEMBERS
//------------------------------------------------------------------------------

//! ctor
pcrxml::PCDATAElement::PCDATAElement():
   Element(),
   d_asCDATASection(false)
{
}


//! ctor from DOM
/*!
   \throws  com::BadStreamFormat if no valid contents
 */
pcrxml::PCDATAElement::PCDATAElement(const QDomElement& n,
                                     const std::string& elementName):
  Element(n, elementName),
  d_asCDATASection(false)
{
  /*
  Calling normalize() on an element converts all its children into a
  standard form. This means that adjacent QDomText objects will be merged
  into a single text object (QDomCDATASection nodes are not merged).

  but no need, need to merge anyway
  n.normalize();
  */

  QDomNodeList c(n.childNodes());
  for(size_t i=0; i < c.length(); ++i) {
   if (c.item(i).isCDATASection()) {
     QDomCDATASection d(c.item(i).toCDATASection());
     d_contents+= asString(d.data());
     continue;
   }
   if (c.item(i).isText()) {
     QDomText d(c.item(i).toText());
     d_contents+= asString(d.data());
     continue;
   }
   throw com::BadStreamFormat("expected empty or simple content model");
  }
}

//! ctor from sections' contents
pcrxml::PCDATAElement::PCDATAElement(const std::string& contents):
  d_contents(contents),
  d_asCDATASection(false)
{
}

//! copy ctor
pcrxml::PCDATAElement::PCDATAElement(const PCDATAElement& src):
  Element(src),
  d_contents(src.d_contents),
  d_asCDATASection(src.d_asCDATASection)
{
}

//! dtor
pcrxml::PCDATAElement::~PCDATAElement()
{
}


//! derived class needs this to construct QDomElement
QDomNode pcrxml::PCDATAElement::contentsNode(const QDomNode& parent) const
{
   QDomDocument doc(parent.ownerDocument());
   QString c(asQString(d_contents));
   POSTCOND(!doc.isNull());

   if (d_asCDATASection)
    return doc.createCDATASection(c);
   else
    return doc.createTextNode(c);
}

//! set value of d_asCDATASection
void pcrxml::PCDATAElement::setAsCDATASection(bool asCDATASection)
{
  d_asCDATASection=asCDATASection;
}

//! get value of d_asCDATASection
bool pcrxml::PCDATAElement::asCDATASection() const
{
  return d_asCDATASection;
}

//! set value of d_contents
void pcrxml::PCDATAElement::setContents(const std::string& contents)
{
  d_contents=contents;
}

//! get value of d_contents
const std::string& pcrxml::PCDATAElement::contents() const
{
  return d_contents;
}


//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



