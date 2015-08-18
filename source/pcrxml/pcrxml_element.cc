#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_PCRXML_ELEMENT
#include "pcrxml_element.h"
#endif

#ifndef  INCLUDED_QDOM
#include <qdom.h>
#define  INCLUDED_QDOM
#endif
#ifndef INCLUDED_PCRXML_STRINGCONV
#include "pcrxml_stringconv.h"
#define INCLUDED_PCRXML_STRINGCONV
#endif

#ifndef INCLUDED_FSTREAM
#include <fstream>
#define INCLUDED_FSTREAM
#endif

#ifndef INCLUDED_COM_FILE
#include "com_file.h"
#define INCLUDED_COM_FILE
#endif

#ifndef INCLUDED_PCRXML_DOCTYPE
#include "pcrxml_doctype.h"
#define INCLUDED_PCRXML_DOCTYPE
#endif

#ifndef INCLUDED_PCRXML_OSTREAM
#include "pcrxml_ostream.h"
#define INCLUDED_PCRXML_OSTREAM
#endif

#ifndef INCLUDED_COM_PATHNAME
#include "com_pathname.h"
#define INCLUDED_COM_PATHNAME
#endif

#ifndef INCLUDED_PCRXML_DOCUMENT
#include "pcrxml_document.h"
#define INCLUDED_PCRXML_DOCUMENT
#endif

#ifndef INCLUDED_COM_EXCEPTION
#include "com_exception.h"
#define INCLUDED_COM_EXCEPTION
#endif


/*!
  \file
  This file contains the implementation of the Element class.
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC ELEMENT MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF ELEMENT MEMBERS
//------------------------------------------------------------------------------

//! empty ctor
pcrxml::Element::Element()
{
}

//! copy ctor
/*!
 * only implemented to have similarty between an
 * Element (not simple content model) and PCDATAElement (simple
 * content).
 */
pcrxml::Element::Element(const Element&)
{
}

//! ctor from QDomNode
/*!
 * checks if initializing with correct element named as expected
 */
pcrxml::Element::Element(
    const QDomElement& e,
    const std::string& elementName)
{
 if (!e.isElement() || elementName != asString(e.nodeName()))
    throw com::BadStreamFormat("initializing element "+elementName+" with wrong element "
                                +asString(e.nodeName()));
}

//! dtor
pcrxml::Element::~Element()
{
}



void pcrxml::Element::write(
  std::string const& fileName) const
{
  write(com::PathName(fileName));
}



//! write element and its subtree to \a fileName
/*! Is is assumed that element and its subtree is valid.
    - prepends DOCTYPE and reference to pcraster.dtd
    - in build type DEVELOP and reads back the file and
      validates it, POSTCOND(FALSE)
    \throws com::OpenFileError or generic IO error
    \returns documentElement
 */
void pcrxml::Element::write(const com::PathName& fileName) const
{
 std::ofstream fs;
 com::open(fs,fileName);
/* Qt already writes a docType with QDomDocument.save()
 * fs << DocType(elementName()).asString();
 * fs << "\n";
 */

 QDomDocument outDoc(toDomDocument());
 fs << outDoc;
 fs << "\n";
 fs.close();

#ifdef DEBUG_DEVELOP
 {
  try {
   com::PathName pn(fileName);
   Document readBack(pn);
  } catch (const com::Exception& e) {
   std::cerr <<  e.getMessages();
   const bool theWrittenXMLFileIsValid(false);
   POSTCOND(theWrittenXMLFileIsValid);
  } catch (...) {
   const bool theWrittenXMLFileIsValid(false);
   POSTCOND(theWrittenXMLFileIsValid);
  }
 }
#endif

}

//! wrapper for write(const com::PathName& fileName)
void pcrxml::Element::writeToFile(const char* fileName) const
{
  write(com::PathName(fileName));
}

//! as a dom document, pcr namespace aware
pcrxml::Document pcrxml::Element::toDomDocument() const
{
  Document doc=createPcrDocument(elementName());
  fill(doc.documentElement());
  return doc;
}

QDomElement pcrxml::Element::toDomElement(QDomDocument doc) const
{
 QDomElement el(doc.createElement(QString(elementName().c_str())));
 fill(el);
 return el;
}

std::string pcrxml::Element::toString() const
{
 pcrxml::Document doc(toDomDocument());
 return doc.toStdString();
}

void pcrxml::Element::appendTo(QDomNode& parent) const
{
  parent.appendChild(toDomElement(parent.ownerDocument()));
}


//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

