/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#ifndef INCLUDED_PCRGENXML_DIMENSION
#define INCLUDED_PCRGENXML_DIMENSION


#ifndef INCLUDED_PCRGENXML_DIMENSIONBASEENUM
#include "pcrgenxml_dimensionbaseenum.h"
#define INCLUDED_PCRGENXML_DIMENSIONBASEENUM
#endif



#ifndef INCLUDED_PCRXML_INTEGER
#include "pcrxml_integer.h"
#define INCLUDED_PCRXML_INTEGER
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
class Dimension : public Element {
private:

 //! clean up
 void clean();
 //! name of the element
 static const std::string d_elementName;

 Dimension& operator=(const Dimension&);

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
 Dimension(const QDomElement& element);
 
 //! default ctor, initialize with no subelement and no attributes
 Dimension();
 //! Copy constructor.
 Dimension(const Dimension&);
 //! dtor
 ~Dimension();
 //! element name
 const std::string& elementName()const;

 //! attribute
 DimensionBaseEnum base;
 //! attribute
 Integer power;
};
} // namespace

#endif
