/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#ifndef INCLUDED_PCRGENXML_MODELRUNSTATUS
#define INCLUDED_PCRGENXML_MODELRUNSTATUS


#ifndef INCLUDED_PCRGENXML_RESULTLASTRUN
#include "pcrgenxml_resultlastrun.h"
#define INCLUDED_PCRGENXML_RESULTLASTRUN
#endif



#ifndef INCLUDED_PCRGENXML_USERDERIVE
#include "pcrgenxml_userderive.h"
#define INCLUDED_PCRGENXML_USERDERIVE
#endif



#ifndef INCLUDED_PCRXML_BOOLEAN
#include "pcrxml_boolean.h"
#define INCLUDED_PCRXML_BOOLEAN
#endif



#ifndef INCLUDED_PCRXML_POSITIVEINTEGER
#include "pcrxml_positiveinteger.h"
#define INCLUDED_PCRXML_POSITIVEINTEGER
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
class ModelRunStatus : public Element {
private:

 //! clean up
 void clean();
 //! name of the element
 static const std::string d_elementName;

 ModelRunStatus& operator=(const ModelRunStatus&);

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
 ModelRunStatus(const QDomElement& element);
 
 //! default ctor, initialize with no subelement and no attributes
 ModelRunStatus();
 //! Copy constructor.
 ModelRunStatus(const ModelRunStatus&);
 //! dtor
 ~ModelRunStatus();
 //! element name
 const std::string& elementName()const;

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
