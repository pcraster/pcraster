#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_PCRXML_DOCUMENT
#include "pcrxml_document.h"
#endif

#ifndef INCLUDED_QXML
#include <qxml.h>
#define INCLUDED_QXML
#endif

#ifndef INCLUDED_COM_PATHINFO
#include "com_pathinfo.h"
#define INCLUDED_COM_PATHINFO
#endif

#ifndef INCLUDED_COM_PATHNAME
#include "com_pathname.h"
#define INCLUDED_COM_PATHNAME
#endif
#ifndef INCLUDED_COM_FILE
#include "com_file.h"
#define INCLUDED_COM_FILE
#endif
#ifndef INCLUDED_QFILE
#include <qfile.h>
#define INCLUDED_QFILE
#endif
#ifndef INCLUDED_COM_STRLIB
#include "com_strlib.h"
#define INCLUDED_COM_STRLIB
#endif
#ifndef INCLUDED_COM_EXCEPTION
#include "com_exception.h"
#define INCLUDED_COM_EXCEPTION
#endif

#ifndef INCLUDED_PCRXML_DOM
#include "pcrxml_dom.h"
#define INCLUDED_PCRXML_DOM
#endif

/*!
  \file
  This file contains the implementation of the Document class.
*/


//------------------------------------------------------------------------------
// DEFINITION OF STATIC DOCUMENT MEMBERS
//------------------------------------------------------------------------------

namespace pcrxml {
//! create with empty Document Element
static Document createEmptyDocElPcrDocument(
    QString const& docElName)
{
 QDomImplementation i;
 Document doc(i.createDocument(
     "http://www.pcraster.nl/xml",
     docElName,
     i.createDocumentType(
      docElName,
      "-//PCRaster//Generic" ,
      "pcraster.dtd")));
 QDomNode decl=doc.createProcessingInstruction("xml",
     "version='1.0' encoding='ISO-8859-1' standalone='yes'");
 doc.insertBefore(decl,doc.firstChild());
 return doc;
}
}

//! create Document with PCRaster stuff
/*!
 * \param contents either a single element name or an Element with tree. For example, "X" will create a document with an empty X element, "<X a='t'><TT/></X>" will create a document with the tree contents as contents.
 *
 * Both the DOCTYPE and xmlns attribute are set to support PCRaster.
 * Recognition of the type of contents is done on base of finding a '<'-character, e.g. start of element.
 */
pcrxml::Document pcrxml::createPcrDocument(
    const std::string& contents)
{
  if (contents.find("<") == std::string::npos) {
   return createEmptyDocElPcrDocument(QString(contents.c_str()));
  }
  Document contentsDoc;

  /* setContent Qt 4.2
  reads the XML document from the string text, returning true if the
  content was successfully parsed; otherwise returns false. Since text
  is already a Unicode string, no encoding detection is performed.

  No namespace processing is performed either.
  */
  contentsDoc.setContent(QString(contents.c_str()));
  QDomElement docEl(contentsDoc.documentElement());
  Document doc(createEmptyDocElPcrDocument(docEl.tagName()));
  QDomNamedNodeMap attrs(docEl.attributes());
  for(size_t i=0; i < attrs.length(); ++i) {
    QDomAttr a=doc.importNode(attrs.item(i),true).toAttr();
    if (a.nodeName()!="xmlns") // seems xmlns is not as an attribute recognized
     if (!doc.documentElement().hasAttribute(a.nodeName())) {
       doc.documentElement().setAttributeNode(a);
     }
  }
  QDomNodeList dnl(docEl.childNodes());
  for(size_t i=0; i < dnl.length(); ++i) {
      QDomNode n(doc.importNode(dnl.item(i),true));
      doc.documentElement().appendChild(n);
  }
  return doc;
}


//------------------------------------------------------------------------------
// DEFINITION OF DOCUMENT MEMBERS
//------------------------------------------------------------------------------

//! common ctor code
/*!
   \todo
    can do line and column numbers on error msg
   \todo
    I guess it is sensible to check here that the HAS a documentElement
 */
void pcrxml::Document::initFromStr(const QString&       content)
{
   QString errMsg;
   bool success = setContent(content,false,&errMsg);
   if (!success) {
     QByteArray asciiData = errMsg.toLatin1();
     const char *goodData = asciiData.constData();
     throw com::BadStreamFormat(goodData);
     }
}

//! Create Dom document from a string (in memory)
/*!
   \throws
      com::BadStreamFormat if xml is not valid
 */
pcrxml::Document::Document(const QString&       content)
{
  initFromStr(content);
}

pcrxml::Document::Document(const QDomDocument& doc):
  QDomDocument(doc)
{
}

pcrxml::Document::Document()
{
}

//! Identical to pcrxml::Document::Document(const QString& content)
pcrxml::Document::Document(const char*content)
{
  initFromStr(content);
}

//! Create Dom document from a file with xml contents
/*!
   \throws
    com::FileError if file not found etc.
   \throws
    com::BadStreamFormat if xml is not valid, <b>without</b>
    the name of \a file in the message WHY?
   \todo
    can do line and column numbers on error msg
*/
pcrxml::Document::Document(const com::PathName& file)
{
  com::PathInfo(file).testOpenForReading();
  QFile f(QString(file.toString().c_str()));
  bool success = f.open(QIODevice::ReadOnly);
  // com::testOpenForReading() should assert this
  // but it does not:
  //  POSTCOND(success);
  if (!success)
    throw com::OpenFileError(file.toString(),com::E_ACCESREAD);

  QString errMsg;
  success = setContent(&f,false,&errMsg);
  if (!success) {
     QByteArray asciiData = errMsg.toLatin1();
     const char *goodData = asciiData.constData();
     throw com::BadStreamFormat(goodData);
  }
  POSTCOND(success); // if nothing thrown then ok here
}

//! a wrapper on QDomDocument::toString()
void pcrxml::Document::write(const com::PathName& file) const
{
  com::PathInfo(file).testOpenForWriting();
  com::write(toStdString(),file);
}

//! a wrapper on QDomDocument::toString()
/*!
 *  Qt4 no longer casts QString to std::string
 */
std::string pcrxml::Document::toStdString() const
{
 return contentsAsString(*this);
}

std::string  pcrxml::contentsAsString( QDomDocument const& doc)
{
 return std::string(doc.toString().toLatin1());
}

//! dtor
pcrxml::Document::~Document()
{
}

//! find first element with name \a tagName
/*!
 * it will find the first one in preorder traversal, is as
 * DOM elementsByTagName method. 
 *
 * \todo
 *  This implementation will also
 *  return the root of the tree if that one matches, QT elementsByTagName
 *  does this not. BUT the free function firstMatchByTagName does NOT. irritating.
 *
 * \param   tagName the name to search for
 * \pre     tagName not empty
 * \returns the element found, if not found a null element
 */
QDomElement pcrxml::Document::firstMatchByTagName(
    const QString& tagName) const
{
  PRECOND(!tagName.isEmpty());

  QDomElement de(documentElement());

  // QDomElement::elementsByTagName only return descendant elements!
  // not the element if the element itself matches
  if (de.tagName() == tagName)
          return de;

  return pcrxml::firstMatchByTagName(de, tagName);
}





//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



