/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#ifndef INCLUDED_PCRGENXML_PARAMETERPANEL
#define INCLUDED_PCRGENXML_PARAMETERPANEL


#ifndef INCLUDED_PCRGENXML_PARAMETERITEM
#include "pcrgenxml_parameteritem.h"
#define INCLUDED_PCRGENXML_PARAMETERITEM
#endif



#ifndef INCLUDED_PCRXML_BOOLEAN
#include "pcrxml_boolean.h"
#define INCLUDED_PCRXML_BOOLEAN
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
class ParameterPanel : public Element {
private:

 //! clean up
 void clean();
 //! name of the element
 static const std::string d_elementName;

 ParameterPanel& operator=(const ParameterPanel&);

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
 ParameterPanel(const QDomElement& element);
 
 //! default ctor, initialize with no subelement and no attributes
 ParameterPanel();
 //! Copy constructor.
 ParameterPanel(const ParameterPanel&);
 //! dtor
 ~ParameterPanel();
 //! element name
 const std::string& elementName()const;

 //! attribute
 String name;
 //! attribute
 Boolean show;
 //! child element
 std::vector<ParameterItem *> parameterItem;
};
} // namespace

#endif
