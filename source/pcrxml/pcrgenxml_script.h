/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#ifndef INCLUDED_PCRGENXML_SCRIPT
#define INCLUDED_PCRGENXML_SCRIPT


#ifndef INCLUDED_PCRGENXML_INTEGERTIMER
#include "pcrgenxml_integertimer.h"
#define INCLUDED_PCRGENXML_INTEGERTIMER
#endif



#ifndef INCLUDED_PCRGENXML_IOSTRATEGY
#include "pcrgenxml_iostrategy.h"
#define INCLUDED_PCRGENXML_IOSTRATEGY
#endif



#ifndef INCLUDED_PCRGENXML_SCRIPTDATA
#include "pcrgenxml_scriptdata.h"
#define INCLUDED_PCRGENXML_SCRIPTDATA
#endif



#ifndef INCLUDED_PCRGENXML_SCRIPTTYPE
#include "pcrgenxml_scripttype.h"
#define INCLUDED_PCRGENXML_SCRIPTTYPE
#endif



#ifndef INCLUDED_PCRXML_STRING
#include "pcrxml_string.h"
#define INCLUDED_PCRXML_STRING
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
class Script : public Element {
private:

 //! clean up
 void clean();
 //! name of the element
 static const std::string d_elementName;

 Script& operator=(const Script&);

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
 Script(const QDomElement& element);
 
 //! default ctor, initialize with no subelement and no attributes
 Script();
 //! Copy constructor.
 Script(const Script&);
 //! dtor
 ~Script();
 //! element name
 const std::string& elementName()const;

 //! attribute
 String scriptFileName;
 //! attribute
 ScriptType scriptType;
 //! attribute
 IoStrategy ioStrategy;
 //! child element
 IntegerTimer *integerTimer;
 //! child element
 ScriptData *scriptData;
};
} // namespace

#endif
