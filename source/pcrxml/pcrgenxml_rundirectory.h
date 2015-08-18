/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#ifndef INCLUDED_PCRGENXML_RUNDIRECTORY
#define INCLUDED_PCRGENXML_RUNDIRECTORY


#ifndef INCLUDED_PCRGENXML_MODELRUNSETTINGS
#include "pcrgenxml_modelrunsettings.h"
#define INCLUDED_PCRGENXML_MODELRUNSETTINGS
#endif



#ifndef INCLUDED_PCRGENXML_MODELRUNSTATUS
#include "pcrgenxml_modelrunstatus.h"
#define INCLUDED_PCRGENXML_MODELRUNSTATUS
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
class RunDirectory : public Element {
private:

 //! clean up
 void clean();
 //! name of the element
 static const std::string d_elementName;

 RunDirectory& operator=(const RunDirectory&);

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
 RunDirectory(const QDomElement& element);
 
 //! default ctor, initialize with no subelement and no attributes
 RunDirectory();
 //! Copy constructor.
 RunDirectory(const RunDirectory&);
 //! dtor
 ~RunDirectory();
 //! element name
 const std::string& elementName()const;

 //! child element
 ModelRunSettings *modelRunSettings;
 //! child element
 ModelRunStatus *modelRunStatus;
};
} // namespace

#endif
