/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#ifndef INCLUDED_PCRGENXML_MAP
#define INCLUDED_PCRGENXML_MAP

#include "pcrgenxml_datatypedtd.h"
#include "pcrxml_element.h"

#include <string>


class QDomNode;
namespace pcrxml {
 //! Do not edit, generated from PCRaster dtd/schema pcrxml::Element has detailed documentation
class Map : public Element {
private:

 //! clean up
 void clean();
 //! name of the element
 static const std::string d_elementName;

 Map& operator=(const Map&);

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
 Map(const QDomElement& element);
 
 //! default ctor, initialize with no subelement and no attributes
 Map();
 //! Copy constructor.
 Map(const Map&);
 //! dtor
 ~Map() override;
 //! element name
 const std::string& elementName()const override;

 //! child element
 DataTypeDTD *dataTypeDTD{nullptr};
};
} // namespace

#endif
