/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#ifndef INCLUDED_PCRGENXML_MAP
#define INCLUDED_PCRGENXML_MAP


#ifndef INCLUDED_PCRGENXML_DATATYPEDTD
#include "pcrgenxml_datatypedtd.h"
#define INCLUDED_PCRGENXML_DATATYPEDTD
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
class Map : public Element {
private:

 //! clean up
 void clean();
 //! name of the element
 static const std::string d_elementName;

 Map& operator=(const Map&);

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
 Map(const QDomElement& element);
 
 //! default ctor, initialize with no subelement and no attributes
 Map();
 //! Copy constructor.
 Map(const Map&);
 //! dtor
 ~Map();
 //! element name
 const std::string& elementName()const;

 //! child element
 DataTypeDTD *dataTypeDTD;
};
} // namespace

#endif
