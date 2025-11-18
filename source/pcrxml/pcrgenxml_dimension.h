/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#ifndef INCLUDED_PCRGENXML_DIMENSION
#define INCLUDED_PCRGENXML_DIMENSION

#include "pcrgenxml_dimensionbaseenum.h"
#include "pcrxml_integer.h"
#include "pcrxml_element.h"

#include <string>



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
  void       fill(QDomElement el) const override;
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
 ~Dimension() override;
 //! element name
 const std::string& elementName()const override;

 //! attribute
 DimensionBaseEnum base;
 //! attribute
 Integer power;
};
} // namespace

#endif
