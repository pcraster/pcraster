/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#ifndef INCLUDED_PCRGENXML_EXCHANGEITEM
#define INCLUDED_PCRGENXML_EXCHANGEITEM


#ifndef INCLUDED_PCRGENXML_EXCHANGEDIRECTION
#include "pcrgenxml_exchangedirection.h"
#define INCLUDED_PCRGENXML_EXCHANGEDIRECTION
#endif



#ifndef INCLUDED_PCRGENXML_VARIABLE
#include "pcrgenxml_variable.h"
#define INCLUDED_PCRGENXML_VARIABLE
#endif



#ifndef INCLUDED_PCRXML_POSITIVEINTEGER
#include "pcrxml_positiveinteger.h"
#define INCLUDED_PCRXML_POSITIVEINTEGER
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
class ExchangeItem : public Element {
private:

 //! clean up
 void clean();
 //! name of the element
 static const std::string d_elementName;

 ExchangeItem& operator=(const ExchangeItem&);

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
 ExchangeItem(const QDomElement& element);
 
 //! default ctor, initialize with no subelement and no attributes
 ExchangeItem();
 //! Copy constructor.
 ExchangeItem(const ExchangeItem&);
 //! dtor
 ~ExchangeItem();
 //! element name
 const std::string& elementName()const;

 //! attribute
 ExchangeDirection exchangeDirection;
 //! attribute
 PositiveInteger index;
 //! child element
 Variable *variable;
};
} // namespace

#endif
