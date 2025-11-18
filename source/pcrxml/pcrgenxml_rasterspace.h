/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#ifndef INCLUDED_PCRGENXML_RASTERSPACE
#define INCLUDED_PCRGENXML_RASTERSPACE

#include "pcrxml_double.h"
#include "pcrxml_positiveinteger.h"
#include "pcrxml_element.h"

#include <string>



class QDomNode;
namespace pcrxml {
 //! Do not edit, generated from PCRaster dtd/schema pcrxml::Element has detailed documentation
class RasterSpace : public Element {
private:

 //! clean up
 void clean();
 //! name of the element
 static const std::string d_elementName;

 RasterSpace& operator=(const RasterSpace&);

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
 RasterSpace(const QDomElement& element);
 
 //! default ctor, initialize with no subelement and no attributes
 RasterSpace();
 //! Copy constructor.
 RasterSpace(const RasterSpace&);
 //! dtor
 ~RasterSpace() override;
 //! element name
 const std::string& elementName()const override;

 //! attribute
 PositiveInteger nrRows;
 //! attribute
 PositiveInteger nrCols;
 //! attribute
 Double cellSize;
 //! attribute
 Double xLowerLeftCorner;
 //! attribute
 Double yLowerLeftCorner;
};
} // namespace

#endif
