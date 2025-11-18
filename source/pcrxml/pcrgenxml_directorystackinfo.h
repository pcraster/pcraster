/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#ifndef INCLUDED_PCRGENXML_DIRECTORYSTACKINFO
#define INCLUDED_PCRGENXML_DIRECTORYSTACKINFO

#include "pcrgenxml_datatypeenum.h"
#include "pcrxml_boolean.h"
#include "pcrxml_double.h"
#include "pcrxml_positiveinteger.h"
#include "pcrxml_element.h"

#include <string>



class QDomNode;
namespace pcrxml {
 //! Do not edit, generated from PCRaster dtd/schema pcrxml::Element has detailed documentation
class DirectoryStackInfo : public Element {
private:

 //! clean up
 void clean();
 //! name of the element
 static const std::string d_elementName;

 DirectoryStackInfo& operator=(const DirectoryStackInfo&);

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
 DirectoryStackInfo(const QDomElement& element);
 
 //! default ctor, initialize with no subelement and no attributes
 DirectoryStackInfo();
 //! Copy constructor.
 DirectoryStackInfo(const DirectoryStackInfo&);
 //! dtor
 ~DirectoryStackInfo() override;
 //! element name
 const std::string& elementName()const override;

 //! attribute
 Boolean allMissingValue;
 //! attribute
 Double minimumValue;
 //! attribute
 Double maximumValue;
 //! attribute
 PositiveInteger stackEnd;
 //! attribute
 DataTypeEnum dataTypeDTD;
};
} // namespace

#endif
