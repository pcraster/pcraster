/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#ifndef INCLUDED_PCRGENXML_DATAEXTEND
#define INCLUDED_PCRGENXML_DATAEXTEND

#include "pcrgenxml_coefficient.h"
#include "pcrgenxml_migrationdirection.h"
#include "pcrxml_element.h"

#include <string>
#include <vector>



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
  void       fill(QDomElement el) const override;
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
 ~DataExtend() override;
 //! element name
 const std::string& elementName()const override;

 //! attribute
 MigrationDirection migrationDirection;
 //! child element
 std::vector<Coefficient *> coefficient;
};
} // namespace

#endif
