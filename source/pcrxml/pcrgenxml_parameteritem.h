/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#ifndef INCLUDED_PCRGENXML_PARAMETERITEM
#define INCLUDED_PCRGENXML_PARAMETERITEM


#ifndef INCLUDED_PCRGENXML_FILEINPUT
#include "pcrgenxml_fileinput.h"
#define INCLUDED_PCRGENXML_FILEINPUT
#endif



#ifndef INCLUDED_PCRGENXML_NUMERICINPUT
#include "pcrgenxml_numericinput.h"
#define INCLUDED_PCRGENXML_NUMERICINPUT
#endif



#ifndef INCLUDED_PCRGENXML_SCRIPTLINK
#include "pcrgenxml_scriptlink.h"
#define INCLUDED_PCRGENXML_SCRIPTLINK
#endif



#ifndef INCLUDED_PCRGENXML_SHOWDATA
#include "pcrgenxml_showdata.h"
#define INCLUDED_PCRGENXML_SHOWDATA
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
class ParameterItem : public Element {
private:

 //! clean up
 void clean();
 //! name of the element
 static const std::string d_elementName;

 ParameterItem& operator=(const ParameterItem&);

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
 ParameterItem(const QDomElement& element);
 
 //! default ctor, initialize with no subelement and no attributes
 ParameterItem();
 //! Copy constructor.
 ParameterItem(const ParameterItem&);
 //! dtor
 ~ParameterItem();
 //! element name
 const std::string& elementName()const;

 //! attribute
 String label;
 //! child element
 ScriptLink *scriptLink;
 //! child element
 NumericInput *numericInput;
 //! child element
 FileInput *fileInput;
 //! child element
 ShowData *showData;
};
} // namespace

#endif
