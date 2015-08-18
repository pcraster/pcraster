/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#ifndef INCLUDED_PCRGENXML_DIRECTORYSTACKINFO
#define INCLUDED_PCRGENXML_DIRECTORYSTACKINFO


#ifndef INCLUDED_PCRGENXML_DATATYPEENUM
#include "pcrgenxml_datatypeenum.h"
#define INCLUDED_PCRGENXML_DATATYPEENUM
#endif



#ifndef INCLUDED_PCRXML_BOOLEAN
#include "pcrxml_boolean.h"
#define INCLUDED_PCRXML_BOOLEAN
#endif



#ifndef INCLUDED_PCRXML_DOUBLE
#include "pcrxml_double.h"
#define INCLUDED_PCRXML_DOUBLE
#endif



#ifndef INCLUDED_PCRXML_POSITIVEINTEGER
#include "pcrxml_positiveinteger.h"
#define INCLUDED_PCRXML_POSITIVEINTEGER
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
class DirectoryStackInfo : public Element {
private:

 //! clean up
 void clean();
 //! name of the element
 static const std::string d_elementName;

 DirectoryStackInfo& operator=(const DirectoryStackInfo&);

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
 DirectoryStackInfo(const QDomElement& element);
 
 //! default ctor, initialize with no subelement and no attributes
 DirectoryStackInfo();
 //! Copy constructor.
 DirectoryStackInfo(const DirectoryStackInfo&);
 //! dtor
 ~DirectoryStackInfo();
 //! element name
 const std::string& elementName()const;

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
