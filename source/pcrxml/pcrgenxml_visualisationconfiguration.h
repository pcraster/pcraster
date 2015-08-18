/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#ifndef INCLUDED_PCRGENXML_VISUALISATIONCONFIGURATION
#define INCLUDED_PCRGENXML_VISUALISATIONCONFIGURATION


#ifndef INCLUDED_PCRGENXML_RUNTIMEPLATFORM
#include "pcrgenxml_runtimeplatform.h"
#define INCLUDED_PCRGENXML_RUNTIMEPLATFORM
#endif



#ifndef INCLUDED_PCRGENXML_VISUALISATIONGROUP
#include "pcrgenxml_visualisationgroup.h"
#define INCLUDED_PCRGENXML_VISUALISATIONGROUP
#endif



#ifndef INCLUDED_PCRXML_STRING
#include "pcrxml_string.h"
#define INCLUDED_PCRXML_STRING
#endif



#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif



#ifndef INCLUDED_VECTOR
#include <vector>
#define INCLUDED_VECTOR
#endif



#ifndef INCLUDED_PCRXML_ELEMENT
#include "pcrxml_element.h"
#define INCLUDED_PCRXML_ELEMENT
#endif


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
  void       fill(QDomElement el) const;
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
 ~VisualisationConfiguration();
 //! element name
 const std::string& elementName()const;

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
