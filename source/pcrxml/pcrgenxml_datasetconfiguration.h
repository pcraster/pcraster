/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#ifndef INCLUDED_PCRGENXML_DATASETCONFIGURATION
#define INCLUDED_PCRGENXML_DATASETCONFIGURATION


#ifndef INCLUDED_PCRGENXML_COMPUTE
#include "pcrgenxml_compute.h"
#define INCLUDED_PCRGENXML_COMPUTE
#endif



#ifndef INCLUDED_PCRGENXML_INPUT
#include "pcrgenxml_input.h"
#define INCLUDED_PCRGENXML_INPUT
#endif



#ifndef INCLUDED_PCRGENXML_INTERPOLATE
#include "pcrgenxml_interpolate.h"
#define INCLUDED_PCRGENXML_INTERPOLATE
#endif



#ifndef INCLUDED_PCRGENXML_LODINGS
#include "pcrgenxml_lodings.h"
#define INCLUDED_PCRGENXML_LODINGS
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
class DataSetConfiguration : public Element {
private:

 //! clean up
 void clean();
 //! name of the element
 static const std::string d_elementName;

 DataSetConfiguration& operator=(const DataSetConfiguration&);

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
 DataSetConfiguration(const QDomElement& element);
 
 //! default ctor, initialize with no subelement and no attributes
 DataSetConfiguration();
 //! Copy constructor.
 DataSetConfiguration(const DataSetConfiguration&);
 //! dtor
 ~DataSetConfiguration();
 //! element name
 const std::string& elementName()const;

 //! attribute
 PositiveInteger version;
 //! child element
 Input *input;
 //! child element
 Compute *compute;
 //! child element
 Interpolate *interpolate;
 //! child element
 Lodings *lodings;
};
} // namespace

#endif
