/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#ifndef INCLUDED_PCRGENXML_NUMERICINPUT
#define INCLUDED_PCRGENXML_NUMERICINPUT

#include "pcrgenxml_lowerlimit.h"
#include "pcrgenxml_upperlimit.h"
#include "pcrxml_element.h"

#include <string>


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
  void       fill(QDomElement el) const override;
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
 ~NumericInput() override;
 //! element name
 const std::string& elementName()const override;

 //! child element
 LowerLimit *lowerLimit{nullptr};
 //! child element
 UpperLimit *upperLimit{nullptr};
};
} // namespace

#endif
