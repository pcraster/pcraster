/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#ifndef INCLUDED_PCRGENXML_SCRIPT
#define INCLUDED_PCRGENXML_SCRIPT

#include "pcrgenxml_integertimer.h"
#include "pcrgenxml_iostrategy.h"
#include "pcrgenxml_scriptdata.h"
#include "pcrgenxml_scripttype.h"
#include "pcrxml_string.h"
#include "pcrxml_element.h"

#include <string>



class QDomNode;
namespace pcrxml {
 //! Do not edit, generated from PCRaster dtd/schema pcrxml::Element has detailed documentation
class Script : public Element {
private:

 //! clean up
 void clean();
 //! name of the element
 static const std::string d_elementName;

 Script& operator=(const Script&);

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
 Script(const QDomElement& element);
 
 //! default ctor, initialize with no subelement and no attributes
 Script();
 //! Copy constructor.
 Script(const Script&);
 //! dtor
 ~Script() override;
 //! element name
 const std::string& elementName()const override;

 //! attribute
 String scriptFileName;
 //! attribute
 ScriptType scriptType;
 //! attribute
 IoStrategy ioStrategy;
 //! child element
 IntegerTimer *integerTimer{nullptr};
 //! child element
 ScriptData *scriptData{nullptr};
};
} // namespace

#endif
