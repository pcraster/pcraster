/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#ifndef INCLUDED_PCRGENXML_INPUTPOINTS
#define INCLUDED_PCRGENXML_INPUTPOINTS

#include "pcrgenxml_computedriveraxis.h"
#include "pcrgenxml_riveraxisfile.h"
#include "pcrxml_double.h"
#include "pcrxml_element.h"

#include <string>



class QDomNode;
namespace pcrxml {
 //! Do not edit, generated from PCRaster dtd/schema pcrxml::Element has detailed documentation
class InputPoints : public Element {
private:

 //! clean up
 void clean();
 //! name of the element
 static const std::string d_elementName;

 InputPoints& operator=(const InputPoints&);

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
 InputPoints(const QDomElement& element);
 
 //! default ctor, initialize with no subelement and no attributes
 InputPoints();
 //! Copy constructor.
 InputPoints(const InputPoints&);
 //! dtor
 ~InputPoints() override;
 //! element name
 const std::string& elementName()const override;

 //! attribute
 Double lodingDistance;
 //! attribute
 Double maxPointDeviation;
 //! child element
 ComputedRiverAxis *computedRiverAxis{nullptr};
 //! child element
 RiverAxisFile *riverAxisFile{nullptr};
};
} // namespace

#endif
