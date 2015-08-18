/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#ifndef INCLUDED_PCRGENXML_NUMERICSETTING
#define INCLUDED_PCRGENXML_NUMERICSETTING


#ifndef INCLUDED_PCRXML_DOUBLE
#include "pcrxml_double.h"
#define INCLUDED_PCRXML_DOUBLE
#endif



#ifndef INCLUDED_PCRXML_STRING
#include "pcrxml_string.h"
#define INCLUDED_PCRXML_STRING
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
class NumericSetting : public Element {
private:

 //! clean up
 void clean();
 //! name of the element
 static const std::string d_elementName;

 NumericSetting& operator=(const NumericSetting&);

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
 NumericSetting(const QDomElement& element);
 
 //! default ctor, initialize with no subelement and no attributes
 NumericSetting();
 //! Copy constructor.
 NumericSetting(const NumericSetting&);
 //! dtor
 ~NumericSetting();
 //! element name
 const std::string& elementName()const;

 //! attribute
 String name;
 //! attribute
 Double value;
};
} // namespace

#endif
