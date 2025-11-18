/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#ifndef INCLUDED_PCRGENXML_USERINTERFACEDESCRIPTION
#define INCLUDED_PCRGENXML_USERINTERFACEDESCRIPTION

#include "pcrgenxml_paneltype.h"
#include "pcrgenxml_parameterpanel.h"
#include "pcrxml_string.h"
#include "pcrxml_element.h"

#include <string>
#include <vector>


class QDomNode;
namespace pcrxml {
 //! Do not edit, generated from PCRaster dtd/schema pcrxml::Element has detailed documentation
class UserInterfaceDescription : public Element {
private:

 //! clean up
 void clean();
 //! name of the element
 static const std::string d_elementName;

 UserInterfaceDescription& operator=(const UserInterfaceDescription&);

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
 UserInterfaceDescription(const QDomElement& element);
 
 //! default ctor, initialize with no subelement and no attributes
 UserInterfaceDescription();
 //! Copy constructor.
 UserInterfaceDescription(const UserInterfaceDescription&);
 //! dtor
 ~UserInterfaceDescription() override;
 //! element name
 const std::string& elementName()const override;

 //! attribute
 String scriptFile;
 //! attribute
 PanelType panelType;
 //! child element
 std::vector<ParameterPanel *> parameterPanel;
};
} // namespace

#endif
