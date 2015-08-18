#ifndef INCLUDED_PCRXML_DOCUMENT
#define INCLUDED_PCRXML_DOCUMENT

#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef  INCLUDED_QDOM
#include <qdom.h>
#define  INCLUDED_QDOM
#endif

#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif

namespace com {
 class PathName;
}

namespace pcrxml {

//! QDocument: A Document keeps a DOM tree, with support of the PCRaster DTD
/*!
 * wrapper on class QDocument
 */
class Document : public QDomDocument
{

private:
  //  Assignment operator. DEFAULT
  // Document&           operator=           (const Document&);

  //  Copy constructor. DEFAULT
  //               Document               (const Document&);

  void initFromStr(const QString&       contents);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                    Document(const QString&       contents);
                    Document(const char*          contents);
                    Document(const com::PathName& file);

                    Document(const QDomDocument& dt);

                    Document();

  /* virtual */    ~Document              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  QDomElement firstMatchByTagName(const QString& tagName) const;
  void        write              (const com::PathName& file) const;

  std::string toStdString        () const;

};



//------------------------------------------------------------------------------
// INLINE FUNCIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------

Document         createPcrDocument(const std::string& docElName);
std::string      contentsAsString( QDomDocument const& doc);


} // namespace pcrxml

#endif
