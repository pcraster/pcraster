/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#ifndef INCLUDED_PCRGENXML_LOWERLIMIT
#define INCLUDED_PCRGENXML_LOWERLIMIT


#ifndef INCLUDED_PCRXML_BOOLEAN
#include "pcrxml_boolean.h"
#define INCLUDED_PCRXML_BOOLEAN
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
class LowerLimit : public Element {
private:

 //! clean up
 void clean();
 //! name of the element
 static const std::string d_elementName;

 LowerLimit& operator=(const LowerLimit&);

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
 LowerLimit(const QDomElement& element);
 
 //! default ctor, initialize with no subelement and no attributes
 LowerLimit();
 //! Copy constructor.
 LowerLimit(const LowerLimit&);
 //! dtor
 ~LowerLimit();
 //! element name
 const std::string& elementName()const;

 //! attribute
 Double value;
 //! attribute
 Boolean inclusive;
};
} // namespace

#endif
