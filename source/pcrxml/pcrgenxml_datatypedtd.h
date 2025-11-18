/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#ifndef INCLUDED_PCRGENXML_DATATYPEDTD
#define INCLUDED_PCRGENXML_DATATYPEDTD

#include "pcrgenxml_datatypeenum.h"
#include "pcrgenxml_dimension.h"
#include "pcrxml_element.h"

#include <string>
#include <vector>



class QDomNode;
namespace pcrxml {
 //! Do not edit, generated from PCRaster dtd/schema pcrxml::Element has detailed documentation
class DataTypeDTD : public Element {
private:

 //! clean up
 void clean();
 //! name of the element
 static const std::string d_elementName;

 DataTypeDTD& operator=(const DataTypeDTD&);

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
 DataTypeDTD(const QDomElement& element);
 
 //! default ctor, initialize with no subelement and no attributes
 DataTypeDTD();
 //! Copy constructor.
 DataTypeDTD(const DataTypeDTD&);
 //! dtor
 ~DataTypeDTD() override;
 //! element name
 const std::string& elementName()const override;

 //! attribute
 DataTypeEnum value;
 //! child element
 std::vector<Dimension *> dimension;
};
} // namespace

#endif
