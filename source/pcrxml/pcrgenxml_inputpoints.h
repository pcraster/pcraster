/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#ifndef INCLUDED_PCRGENXML_INPUTPOINTS
#define INCLUDED_PCRGENXML_INPUTPOINTS


#ifndef INCLUDED_PCRGENXML_COMPUTEDRIVERAXIS
#include "pcrgenxml_computedriveraxis.h"
#define INCLUDED_PCRGENXML_COMPUTEDRIVERAXIS
#endif



#ifndef INCLUDED_PCRGENXML_RIVERAXISFILE
#include "pcrgenxml_riveraxisfile.h"
#define INCLUDED_PCRGENXML_RIVERAXISFILE
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
class InputPoints : public Element {
private:

 //! clean up
 void clean();
 //! name of the element
 static const std::string d_elementName;

 InputPoints& operator=(const InputPoints&);

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
 InputPoints(const QDomElement& element);
 
 //! default ctor, initialize with no subelement and no attributes
 InputPoints();
 //! Copy constructor.
 InputPoints(const InputPoints&);
 //! dtor
 ~InputPoints();
 //! element name
 const std::string& elementName()const;

 //! attribute
 Double lodingDistance;
 //! attribute
 Double maxPointDeviation;
 //! child element
 ComputedRiverAxis *computedRiverAxis;
 //! child element
 RiverAxisFile *riverAxisFile;
};
} // namespace

#endif
