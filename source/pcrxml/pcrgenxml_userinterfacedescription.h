/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#ifndef INCLUDED_PCRGENXML_USERINTERFACEDESCRIPTION
#define INCLUDED_PCRGENXML_USERINTERFACEDESCRIPTION


#ifndef INCLUDED_PCRGENXML_PANELTYPE
#include "pcrgenxml_paneltype.h"
#define INCLUDED_PCRGENXML_PANELTYPE
#endif



#ifndef INCLUDED_PCRGENXML_PARAMETERPANEL
#include "pcrgenxml_parameterpanel.h"
#define INCLUDED_PCRGENXML_PARAMETERPANEL
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
class UserInterfaceDescription : public Element {
private:

 //! clean up
 void clean();
 //! name of the element
 static const std::string d_elementName;

 UserInterfaceDescription& operator=(const UserInterfaceDescription&);

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
 UserInterfaceDescription(const QDomElement& element);
 
 //! default ctor, initialize with no subelement and no attributes
 UserInterfaceDescription();
 //! Copy constructor.
 UserInterfaceDescription(const UserInterfaceDescription&);
 //! dtor
 ~UserInterfaceDescription();
 //! element name
 const std::string& elementName()const;

 //! attribute
 String scriptFile;
 //! attribute
 PanelType panelType;
 //! child element
 std::vector<ParameterPanel *> parameterPanel;
};
} // namespace

#endif
