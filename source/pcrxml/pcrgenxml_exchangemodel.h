/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#ifndef INCLUDED_PCRGENXML_EXCHANGEMODEL
#define INCLUDED_PCRGENXML_EXCHANGEMODEL


#include "pcrgenxml_areamapdtd.h"
#include "pcrgenxml_exchangeitem.h"
#include "pcrgenxml_iostrategytype.h"
#include "pcrgenxml_integertimer.h"
#include "pcrxml_string.h"
#include "pcrxml_element.h"

#include <string>
#include <vector>



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
  void       fill(QDomElement el) const override;
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
 ~ExchangeModel() override;
 //! element name
 const std::string& elementName()const override;

 //! attribute
 String id;
 //! attribute
 IOStrategyType ioStrategy;
 //! attribute
 String description;
 //! child element
 IntegerTimer *integerTimer{nullptr};
 //! child element
 AreaMapDTD *areaMapDTD{nullptr};
 //! child element
 std::vector<ExchangeItem *> exchangeItem;
};
} // namespace

#endif
