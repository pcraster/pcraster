/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#ifndef INCLUDED_PCRGENXML_VISUALISATIONCONFIGURATION
#define INCLUDED_PCRGENXML_VISUALISATIONCONFIGURATION

#include "pcrgenxml_runtimeplatform.h"
#include "pcrgenxml_visualisationgroup.h"
#include "pcrxml_string.h"
#include "pcrxml_element.h"

#include <string>
#include <vector>


class QDomNode;
namespace pcrxml {
 //! Do not edit, generated from PCRaster dtd/schema pcrxml::Element has detailed documentation
class VisualisationConfiguration : public Element {
private:

 //! clean up
 void clean();
 //! name of the element
 static const std::string d_elementName;

 VisualisationConfiguration& operator=(const VisualisationConfiguration&);

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
 VisualisationConfiguration(const QDomElement& element);
 
 //! default ctor, initialize with no subelement and no attributes
 VisualisationConfiguration();
 //! Copy constructor.
 VisualisationConfiguration(const VisualisationConfiguration&);
 //! dtor
 ~VisualisationConfiguration() override;
 //! element name
 const std::string& elementName()const override;

 //! attribute
 String date;
 //! attribute
 String version;
 //! attribute
 String cwd;
 //! attribute
 RuntimePlatform os;
 //! child element
 std::vector<VisualisationGroup *> visualisationGroup;
};
} // namespace

#endif
