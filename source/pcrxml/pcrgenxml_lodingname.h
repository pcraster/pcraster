/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#ifndef INCLUDED_PCRGENXML_LODINGNAME
#define INCLUDED_PCRGENXML_LODINGNAME

#include "pcrxml_string.h"
#include "pcrxml_element.h"

#include <string>



class QDomNode;
namespace pcrxml {
 //! Do not edit, generated from PCRaster dtd/schema pcrxml::Element has detailed documentation
class LodingName : public Element {
private:

 //! clean up
 void clean();
 //! name of the element
 static const std::string d_elementName;

 LodingName& operator=(const LodingName&);

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
 LodingName(const QDomElement& element);
 
 //! default ctor, initialize with no subelement and no attributes
 LodingName();
 //! Copy constructor.
 LodingName(const LodingName&);
 //! dtor
 ~LodingName() override;
 //! element name
 const std::string& elementName()const override;

 //! attribute
 String value;
};
} // namespace

#endif
