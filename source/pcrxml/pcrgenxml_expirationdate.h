/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#ifndef INCLUDED_PCRGENXML_EXPIRATIONDATE
#define INCLUDED_PCRGENXML_EXPIRATIONDATE


#ifndef INCLUDED_PCRGENXML_DATEFORMAT
#include "pcrgenxml_dateformat.h"
#define INCLUDED_PCRGENXML_DATEFORMAT
#endif



#ifndef INCLUDED_PCRXML_DATE
#include "pcrxml_date.h"
#define INCLUDED_PCRXML_DATE
#endif



#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif



#ifndef INCLUDED_PCRXML_ELEMENT
#include "pcrxml_element.h"
#define INCLUDED_PCRXML_ELEMENT
#endif


class QDomNode;
namespace pcrxml {
 //! Do not edit, generated from PCRaster dtd/schema pcrxml::Element has detailed documentation
class ExpirationDate : public Element {
private:

 //! clean up
 void clean();
 //! name of the element
 static const std::string d_elementName;

 ExpirationDate& operator=(const ExpirationDate&);

 //! in support of toDomElement and toDom
  void       fill(QDomElement el) const;
public:
 static const char* tagName() {
   return d_elementName.c_str();
 }
 //! ctor
 /*! \throws
        com::BadStreamFormat if xml is not valid
 */
 ExpirationDate(const QDomElement& element);
 
 //! default ctor, initialize with no subelement and no attributes
 ExpirationDate();
 //! Copy constructor.
 ExpirationDate(const ExpirationDate&);
 //! dtor
 ~ExpirationDate();
 //! element name
 const std::string& elementName()const;

 //! attribute
 Date value;
 //! attribute
 DateFormat dateFormat;
};
} // namespace

#endif
