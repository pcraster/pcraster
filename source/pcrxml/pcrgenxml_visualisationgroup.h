/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#ifndef INCLUDED_PCRGENXML_VISUALISATIONGROUP
#define INCLUDED_PCRGENXML_VISUALISATIONGROUP


#ifndef INCLUDED_PCRGENXML_DATAOBJECT
#include "pcrgenxml_dataobject.h"
#define INCLUDED_PCRGENXML_DATAOBJECT
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
class VisualisationGroup : public Element {
private:

 //! clean up
 void clean();
 //! name of the element
 static const std::string d_elementName;

 VisualisationGroup& operator=(const VisualisationGroup&);

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
 VisualisationGroup(const QDomElement& element);
 
 //! default ctor, initialize with no subelement and no attributes
 VisualisationGroup();
 //! Copy constructor.
 VisualisationGroup(const VisualisationGroup&);
 //! dtor
 ~VisualisationGroup();
 //! element name
 const std::string& elementName()const;

 //! child element
 DataObject *dataObject;
};
} // namespace

#endif
