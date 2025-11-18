/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#ifndef INCLUDED_PCRGENXML_DATASETCONFIGURATION
#define INCLUDED_PCRGENXML_DATASETCONFIGURATION

#include "pcrgenxml_compute.h"
#include "pcrgenxml_input.h"
#include "pcrgenxml_interpolate.h"
#include "pcrgenxml_lodings.h"
#include "pcrxml_positiveinteger.h"
#include "pcrxml_element.h"

#include <string>


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
  void       fill(QDomElement el) const override;
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
 ~DataSetConfiguration() override;
 //! element name
 const std::string& elementName()const override;

 //! attribute
 PositiveInteger version;
 //! child element
 Input *input{nullptr};
 //! child element
 Compute *compute{nullptr};
 //! child element
 Interpolate *interpolate{nullptr};
 //! child element
 Lodings *lodings{nullptr};
};
} // namespace

#endif
