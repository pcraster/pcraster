/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#ifndef INCLUDED_PCRGENXML_EXPIRATIONDATE
#define INCLUDED_PCRGENXML_EXPIRATIONDATE

#include "pcrgenxml_dateformat.h"
#include "pcrxml_date.h"
#include "pcrxml_element.h"

#include <string>


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
  void       fill(QDomElement el) const override;
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
 ~ExpirationDate() override;
 //! element name
 const std::string& elementName()const override;

 //! attribute
 Date value;
 //! attribute
 DateFormat dateFormat;
};
} // namespace

#endif
