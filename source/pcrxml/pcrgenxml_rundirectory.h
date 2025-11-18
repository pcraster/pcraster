/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#ifndef INCLUDED_PCRGENXML_RUNDIRECTORY
#define INCLUDED_PCRGENXML_RUNDIRECTORY

#include "pcrgenxml_modelrunsettings.h"
#include "pcrgenxml_modelrunstatus.h"
#include "pcrxml_element.h"

#include <string>


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
  void       fill(QDomElement el) const override;
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
 ~RunDirectory() override;
 //! element name
 const std::string& elementName()const override;

 //! child element
 ModelRunSettings *modelRunSettings{nullptr};
 //! child element
 ModelRunStatus *modelRunStatus{nullptr};
};
} // namespace

#endif
