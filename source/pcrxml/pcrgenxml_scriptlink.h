/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#ifndef INCLUDED_PCRGENXML_SCRIPTLINK
#define INCLUDED_PCRGENXML_SCRIPTLINK

#include "pcrgenxml_data.h"
#include "pcrxml_element.h"

#include <string>
#include <vector>



class QDomNode;
namespace pcrxml {
 //! Do not edit, generated from PCRaster dtd/schema pcrxml::Element has detailed documentation
class ScriptLink : public Element {
private:

 //! clean up
 void clean();
 //! name of the element
 static const std::string d_elementName;

 ScriptLink& operator=(const ScriptLink&);

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
 ScriptLink(const QDomElement& element);
 
 //! default ctor, initialize with no subelement and no attributes
 ScriptLink();
 //! Copy constructor.
 ScriptLink(const ScriptLink&);
 //! dtor
 ~ScriptLink() override;
 //! element name
 const std::string& elementName()const override;

 //! child element
 std::vector<Data *> data;
};
} // namespace

#endif
