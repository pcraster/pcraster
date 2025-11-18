/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#ifndef INCLUDED_PCRGENXML_INTERPOLATE
#define INCLUDED_PCRGENXML_INTERPOLATE

#include "pcrxml_double.h"
#include "pcrxml_positiveinteger.h"
#include "pcrxml_element.h"

#include <string>



class QDomNode;
namespace pcrxml {
 //! Do not edit, generated from PCRaster dtd/schema pcrxml::Element has detailed documentation
class Interpolate : public Element {
private:

 //! clean up
 void clean();
 //! name of the element
 static const std::string d_elementName;

 Interpolate& operator=(const Interpolate&);

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
 Interpolate(const QDomElement& element);
 
 //! default ctor, initialize with no subelement and no attributes
 Interpolate();
 //! Copy constructor.
 Interpolate(const Interpolate&);
 //! dtor
 ~Interpolate() override;
 //! element name
 const std::string& elementName()const override;

 //! attribute
 Double searchRadius;
 //! attribute
 Double mapCellSize;
 //! attribute
 PositiveInteger minimumPointsEachQuadrant;
};
} // namespace

#endif
