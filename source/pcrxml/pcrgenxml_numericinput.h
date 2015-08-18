/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#ifndef INCLUDED_PCRGENXML_NUMERICINPUT
#define INCLUDED_PCRGENXML_NUMERICINPUT


#ifndef INCLUDED_PCRGENXML_LOWERLIMIT
#include "pcrgenxml_lowerlimit.h"
#define INCLUDED_PCRGENXML_LOWERLIMIT
#endif



#ifndef INCLUDED_PCRGENXML_UPPERLIMIT
#include "pcrgenxml_upperlimit.h"
#define INCLUDED_PCRGENXML_UPPERLIMIT
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
class NumericInput : public Element {
private:

 //! clean up
 void clean();
 //! name of the element
 static const std::string d_elementName;

 NumericInput& operator=(const NumericInput&);

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
 NumericInput(const QDomElement& element);
 
 //! default ctor, initialize with no subelement and no attributes
 NumericInput();
 //! Copy constructor.
 NumericInput(const NumericInput&);
 //! dtor
 ~NumericInput();
 //! element name
 const std::string& elementName()const;

 //! child element
 LowerLimit *lowerLimit;
 //! child element
 UpperLimit *upperLimit;
};
} // namespace

#endif
