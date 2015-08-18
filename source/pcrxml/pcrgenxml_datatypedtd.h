/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#ifndef INCLUDED_PCRGENXML_DATATYPEDTD
#define INCLUDED_PCRGENXML_DATATYPEDTD


#ifndef INCLUDED_PCRGENXML_DATATYPEENUM
#include "pcrgenxml_datatypeenum.h"
#define INCLUDED_PCRGENXML_DATATYPEENUM
#endif



#ifndef INCLUDED_PCRGENXML_DIMENSION
#include "pcrgenxml_dimension.h"
#define INCLUDED_PCRGENXML_DIMENSION
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
class DataTypeDTD : public Element {
private:

 //! clean up
 void clean();
 //! name of the element
 static const std::string d_elementName;

 DataTypeDTD& operator=(const DataTypeDTD&);

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
 DataTypeDTD(const QDomElement& element);
 
 //! default ctor, initialize with no subelement and no attributes
 DataTypeDTD();
 //! Copy constructor.
 DataTypeDTD(const DataTypeDTD&);
 //! dtor
 ~DataTypeDTD();
 //! element name
 const std::string& elementName()const;

 //! attribute
 DataTypeEnum value;
 //! child element
 std::vector<Dimension *> dimension;
};
} // namespace

#endif
