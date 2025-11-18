/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#ifndef INCLUDED_PCRGENXML_PARAMETERPANEL
#define INCLUDED_PCRGENXML_PARAMETERPANEL

#include "pcrgenxml_parameteritem.h"
#include "pcrxml_boolean.h"
#include "pcrxml_string.h"
#include "pcrxml_element.h"

#include <string>
#include <vector>



class QDomNode;
namespace pcrxml {
 //! Do not edit, generated from PCRaster dtd/schema pcrxml::Element has detailed documentation
class ParameterPanel : public Element {
private:

 //! clean up
 void clean();
 //! name of the element
 static const std::string d_elementName;

 ParameterPanel& operator=(const ParameterPanel&);

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
 ParameterPanel(const QDomElement& element);
 
 //! default ctor, initialize with no subelement and no attributes
 ParameterPanel();
 //! Copy constructor.
 ParameterPanel(const ParameterPanel&);
 //! dtor
 ~ParameterPanel() override;
 //! element name
 const std::string& elementName()const override;

 //! attribute
 String name;
 //! attribute
 Boolean show;
 //! child element
 std::vector<ParameterItem *> parameterItem;
};
} // namespace

#endif
