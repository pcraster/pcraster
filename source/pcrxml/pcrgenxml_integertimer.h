/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#ifndef INCLUDED_PCRGENXML_INTEGERTIMER
#define INCLUDED_PCRGENXML_INTEGERTIMER

#include "pcrxml_positiveinteger.h"
#include "pcrxml_element.h"

#include <string>



class QDomNode;
namespace pcrxml {
 //! Do not edit, generated from PCRaster dtd/schema pcrxml::Element has detailed documentation
class IntegerTimer : public Element {
private:

 //! clean up
 void clean();
 //! name of the element
 static const std::string d_elementName;

 IntegerTimer& operator=(const IntegerTimer&);

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
 IntegerTimer(const QDomElement& element);
 
 //! default ctor, initialize with no subelement and no attributes
 IntegerTimer();
 //! Copy constructor.
 IntegerTimer(const IntegerTimer&);
 //! dtor
 ~IntegerTimer() override;
 //! element name
 const std::string& elementName()const override;

 //! attribute
 PositiveInteger start;
 //! attribute
 PositiveInteger end;
 //! attribute
 PositiveInteger step;
};
} // namespace

#endif
