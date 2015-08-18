/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#ifndef INCLUDED_PCRGENXML_COEFFICIENT
#define INCLUDED_PCRGENXML_COEFFICIENT


#ifndef INCLUDED_PCRXML_BINDOUBLELE
#include "pcrxml_bindoublele.h"
#define INCLUDED_PCRXML_BINDOUBLELE
#endif



#ifndef INCLUDED_PCRXML_DOUBLE
#include "pcrxml_double.h"
#define INCLUDED_PCRXML_DOUBLE
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
class Coefficient : public Element {
private:

 //! clean up
 void clean();
 //! name of the element
 static const std::string d_elementName;

 Coefficient& operator=(const Coefficient&);

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
 Coefficient(const QDomElement& element);
 
 //! default ctor, initialize with no subelement and no attributes
 Coefficient();
 //! Copy constructor.
 Coefficient(const Coefficient&);
 //! dtor
 ~Coefficient();
 //! element name
 const std::string& elementName()const;

 //! attribute
 Double value;
 //! attribute
 BinDoubleLE binValue;
};
} // namespace

#endif
