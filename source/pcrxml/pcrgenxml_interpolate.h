/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#ifndef INCLUDED_PCRGENXML_INTERPOLATE
#define INCLUDED_PCRGENXML_INTERPOLATE


#ifndef INCLUDED_PCRXML_DOUBLE
#include "pcrxml_double.h"
#define INCLUDED_PCRXML_DOUBLE
#endif



#ifndef INCLUDED_PCRXML_POSITIVEINTEGER
#include "pcrxml_positiveinteger.h"
#define INCLUDED_PCRXML_POSITIVEINTEGER
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
class Interpolate : public Element {
private:

 //! clean up
 void clean();
 //! name of the element
 static const std::string d_elementName;

 Interpolate& operator=(const Interpolate&);

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
 Interpolate(const QDomElement& element);
 
 //! default ctor, initialize with no subelement and no attributes
 Interpolate();
 //! Copy constructor.
 Interpolate(const Interpolate&);
 //! dtor
 ~Interpolate();
 //! element name
 const std::string& elementName()const;

 //! attribute
 Double searchRadius;
 //! attribute
 Double mapCellSize;
 //! attribute
 PositiveInteger minimumPointsEachQuadrant;
};
} // namespace

#endif
