/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#ifndef INCLUDED_PCRGENXML_PARAMETERITEM
#define INCLUDED_PCRGENXML_PARAMETERITEM

#include "pcrgenxml_fileinput.h"
#include "pcrgenxml_numericinput.h"
#include "pcrgenxml_scriptlink.h"
#include "pcrgenxml_showdata.h"
#include "pcrxml_string.h"
#include "pcrxml_element.h"

#include <string>



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
  void       fill(QDomElement el) const override;
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
 ~ParameterItem() override;
 //! element name
 const std::string& elementName()const override;

 //! attribute
 String label;
 //! child element
 ScriptLink *scriptLink{nullptr};
 //! child element
 NumericInput *numericInput{nullptr};
 //! child element
 FileInput *fileInput{nullptr};
 //! child element
 ShowData *showData{nullptr};
};
} // namespace

#endif
