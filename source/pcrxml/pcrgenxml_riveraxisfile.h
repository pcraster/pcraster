/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#ifndef INCLUDED_PCRGENXML_RIVERAXISFILE
#define INCLUDED_PCRGENXML_RIVERAXISFILE

#include "pcrxml_string.h"
#include "pcrxml_element.h"

#include <string>



class QDomNode;
namespace pcrxml {
 //! Do not edit, generated from PCRaster dtd/schema pcrxml::Element has detailed documentation
class RiverAxisFile : public Element {
private:

 //! clean up
 void clean();
 //! name of the element
 static const std::string d_elementName;

 RiverAxisFile& operator=(const RiverAxisFile&);

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
 RiverAxisFile(const QDomElement& element);
 
 //! default ctor, initialize with no subelement and no attributes
 RiverAxisFile();
 //! Copy constructor.
 RiverAxisFile(const RiverAxisFile&);
 //! dtor
 ~RiverAxisFile() override;
 //! element name
 const std::string& elementName()const override;

 //! attribute
 String value;
};
} // namespace

#endif
