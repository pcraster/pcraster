/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#ifndef INCLUDED_PCRGENXML_DATAEXTEND
#define INCLUDED_PCRGENXML_DATAEXTEND


#ifndef INCLUDED_PCRGENXML_COEFFICIENT
#include "pcrgenxml_coefficient.h"
#define INCLUDED_PCRGENXML_COEFFICIENT
#endif



#ifndef INCLUDED_PCRGENXML_MIGRATIONDIRECTION
#include "pcrgenxml_migrationdirection.h"
#define INCLUDED_PCRGENXML_MIGRATIONDIRECTION
#endif



#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif



#ifndef INCLUDED_VECTOR
#include <vector>
#define INCLUDED_VECTOR
#endif



#ifndef INCLUDED_PCRXML_ELEMENT
#include "pcrxml_element.h"
#define INCLUDED_PCRXML_ELEMENT
#endif


class QDomNode;
namespace pcrxml {
 //! Do not edit, generated from PCRaster dtd/schema pcrxml::Element has detailed documentation
class DataExtend : public Element {
private:

 //! clean up
 void clean();
 //! name of the element
 static const std::string d_elementName;

 DataExtend& operator=(const DataExtend&);

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
 DataExtend(const QDomElement& element);
 
 //! default ctor, initialize with no subelement and no attributes
 DataExtend();
 //! Copy constructor.
 DataExtend(const DataExtend&);
 //! dtor
 ~DataExtend();
 //! element name
 const std::string& elementName()const;

 //! attribute
 MigrationDirection migrationDirection;
 //! child element
 std::vector<Coefficient *> coefficient;
};
} // namespace

#endif
