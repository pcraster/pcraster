/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#ifndef INCLUDED_PCRGENXML_COMPUTE
#define INCLUDED_PCRGENXML_COMPUTE

#include "pcrxml_double.h"
#include "pcrxml_element.h"

#include <string>



class QDomNode;
namespace pcrxml {
 //! Do not edit, generated from PCRaster dtd/schema pcrxml::Element has detailed documentation
class Compute : public Element {
private:

 //! clean up
 void clean();
 //! name of the element
 static const std::string d_elementName;

 Compute& operator=(const Compute&);

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
 Compute(const QDomElement& element);
 
 //! default ctor, initialize with no subelement and no attributes
 Compute();
 //! Copy constructor.
 Compute(const Compute&);
 //! dtor
 ~Compute() override;
 //! element name
 const std::string& elementName()const override;

 //! attribute
 Double maximalMigrationDistance;
 //! attribute
 Double maximumZeroDownCrossingThreshold;
 //! attribute
 Double minimalSlopeBrinkPoint;
 //! attribute
 Double minimalBrinkHeight;
};
} // namespace

#endif
