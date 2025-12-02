#include "stddefx.h"
#include "pcrxml_element.h"
#include "pcrxml_stringconv.h"
#include "com_file.h"
#include "pcrxml_doctype.h"
#include "pcrxml_ostream.h"
#include "com_pathname.h"
#include "pcrxml_document.h"
#include "com_exception.h"

#include <QDomDocument>

#include <fstream>

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
pcrxml::Element::Element(const Element &)
{
}

//! ctor from QDomNode
/*!
 * checks if initializing with correct element named as expected
 */
pcrxml::Element::Element(const QDomElement &e, const std::string &elementName)
{
  if (!e.isElement() || elementName != asString(e.nodeName()))
    throw com::BadStreamFormat("initializing element " + elementName + " with wrong element " +
                               asString(e.nodeName()));
}

//! dtor
pcrxml::Element::~Element()
{
}

void pcrxml::Element::write(std::string const &fileName) const
{
  write(com::PathName(fileName));
}

//! write element and its subtree to \a fileName
/*! Is is assumed that element and its subtree is valid.
    - prepends DOCTYPE and reference to pcraster.dtd
    - in build type DEVELOP and reads back the file and
      validates it, POSTCOND(false)
    \throws com::OpenFileError or generic IO error
    \returns documentElement
 */
void pcrxml::Element::write(const com::PathName &fileName) const
{
  std::ofstream fs;
  com::open(fs, fileName);
  /* Qt already writes a docType with QDomDocument.save()
 * fs << DocType(elementName()).asString();
 * fs << "\n";
 */

  QDomDocument const outDoc(toDomDocument());
  fs << outDoc;
  fs << "\n";
  fs.close();

#ifdef DEBUG_DEVELOP
  {
    try {
      const com::PathName &pn(fileName);
      Document const readBack(pn);
    } catch (const com::Exception &e) {
      std::cerr << e.getMessages();
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
void pcrxml::Element::writeToFile(const char *fileName) const
{
  write(com::PathName(fileName));
}

//! as a dom document, pcr namespace aware
pcrxml::Document pcrxml::Element::toDomDocument() const
{
  Document const doc = createPcrDocument(elementName());
  fill(doc.documentElement());
  return doc;
}

QDomElement pcrxml::Element::toDomElement(QDomDocument doc) const
{
  QDomElement const el(doc.createElement(QString(elementName().c_str())));
  fill(el);
  return el;
}

std::string pcrxml::Element::toString() const
{
  pcrxml::Document const doc(toDomDocument());
  return doc.toStdString();
}

void pcrxml::Element::appendTo(QDomNode &parent) const
{
  parent.appendChild(toDomElement(parent.ownerDocument()));
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------
