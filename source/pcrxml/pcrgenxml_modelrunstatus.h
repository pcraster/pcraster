/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#ifndef INCLUDED_PCRGENXML_MODELRUNSTATUS
#define INCLUDED_PCRGENXML_MODELRUNSTATUS

#include "pcrgenxml_resultlastrun.h"
#include "pcrgenxml_userderive.h"
#include "pcrxml_boolean.h"
#include "pcrxml_positiveinteger.h"
#include "pcrxml_string.h"
#include "pcrxml_element.h"

#include <string>



class QDomNode;
namespace pcrxml {
 //! Do not edit, generated from PCRaster dtd/schema pcrxml::Element has detailed documentation
class ModelRunStatus : public Element {
private:

 //! clean up
 void clean();
 //! name of the element
 static const std::string d_elementName;

 ModelRunStatus& operator=(const ModelRunStatus&);

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
 ModelRunStatus(const QDomElement& element);
 
 //! default ctor, initialize with no subelement and no attributes
 ModelRunStatus();
 //! Copy constructor.
 ModelRunStatus(const ModelRunStatus&);
 //! dtor
 ~ModelRunStatus() override;
 //! element name
 const std::string& elementName()const override;

 //! attribute
 String userInterfaceName;
 //! attribute
 Boolean userCanChange;
 //! attribute
 Boolean userCanRun;
 //! attribute
 Boolean userCanDelete;
 //! attribute
 UserDerive userDerive;
 //! attribute
 ResultLastRun resultLastRun;
 //! attribute
 PositiveInteger lastTimeStep;
};
} // namespace

#endif
