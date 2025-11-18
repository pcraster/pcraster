/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#ifndef INCLUDED_PCRGENXML_EXCHANGEITEM
#define INCLUDED_PCRGENXML_EXCHANGEITEM

#include "pcrgenxml_exchangedirection.h"
#include "pcrgenxml_variable.h"
#include "pcrxml_positiveinteger.h"
#include "pcrxml_element.h"

#include <string>



class QDomNode;
namespace pcrxml {
 //! Do not edit, generated from PCRaster dtd/schema pcrxml::Element has detailed documentation
class ExchangeItem : public Element {
private:

 //! clean up
 void clean();
 //! name of the element
 static const std::string d_elementName;

 ExchangeItem& operator=(const ExchangeItem&);

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
 ExchangeItem(const QDomElement& element);
 
 //! default ctor, initialize with no subelement and no attributes
 ExchangeItem();
 //! Copy constructor.
 ExchangeItem(const ExchangeItem&);
 //! dtor
 ~ExchangeItem() override;
 //! element name
 const std::string& elementName()const override;

 //! attribute
 ExchangeDirection exchangeDirection;
 //! attribute
 PositiveInteger index;
 //! child element
 Variable *variable{nullptr};
};
} // namespace

#endif
