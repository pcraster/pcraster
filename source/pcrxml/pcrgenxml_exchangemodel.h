/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#ifndef INCLUDED_PCRGENXML_EXCHANGEMODEL
#define INCLUDED_PCRGENXML_EXCHANGEMODEL


#ifndef INCLUDED_PCRGENXML_AREAMAPDTD
#include "pcrgenxml_areamapdtd.h"
#define INCLUDED_PCRGENXML_AREAMAPDTD
#endif



#ifndef INCLUDED_PCRGENXML_EXCHANGEITEM
#include "pcrgenxml_exchangeitem.h"
#define INCLUDED_PCRGENXML_EXCHANGEITEM
#endif



#ifndef INCLUDED_PCRGENXML_IOSTRATEGYTYPE
#include "pcrgenxml_iostrategytype.h"
#define INCLUDED_PCRGENXML_IOSTRATEGYTYPE
#endif



#ifndef INCLUDED_PCRGENXML_INTEGERTIMER
#include "pcrgenxml_integertimer.h"
#define INCLUDED_PCRGENXML_INTEGERTIMER
#endif



#ifndef INCLUDED_PCRXML_STRING
#include "pcrxml_string.h"
#define INCLUDED_PCRXML_STRING
#endif



#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif



#ifndef INCLUDED_VECTOR
#include <vector>
#define INCLUDED_VECTOR
#endif



#ifndef INCLUDED_PCRXML_ELEMENT
#include "pcrxml_element.h"
#define INCLUDED_PCRXML_ELEMENT
#endif


class QDomNode;
namespace pcrxml {
 //! Do not edit, generated from PCRaster dtd/schema pcrxml::Element has detailed documentation
class ExchangeModel : public Element {
private:

 //! clean up
 void clean();
 //! name of the element
 static const std::string d_elementName;

 ExchangeModel& operator=(const ExchangeModel&);

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
 ExchangeModel(const QDomElement& element);
 
 //! default ctor, initialize with no subelement and no attributes
 ExchangeModel();
 //! Copy constructor.
 ExchangeModel(const ExchangeModel&);
 //! dtor
 ~ExchangeModel();
 //! element name
 const std::string& elementName()const;

 //! attribute
 String id;
 //! attribute
 IOStrategyType ioStrategy;
 //! attribute
 String description;
 //! child element
 IntegerTimer *integerTimer;
 //! child element
 AreaMapDTD *areaMapDTD;
 //! child element
 std::vector<ExchangeItem *> exchangeItem;
};
} // namespace

#endif
