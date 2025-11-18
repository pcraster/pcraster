/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#ifndef INCLUDED_PCRGENXML_LODINGS
#define INCLUDED_PCRGENXML_LODINGS

#include "pcrgenxml_dataextend.h"
#include "pcrgenxml_lodingname.h"
#include "pcrxml_element.h"

#include <string>
#include <vector>



class QDomNode;
namespace pcrxml {
 //! Do not edit, generated from PCRaster dtd/schema pcrxml::Element has detailed documentation
class Lodings : public Element {
private:

 //! clean up
 void clean();
 //! name of the element
 static const std::string d_elementName;

 Lodings& operator=(const Lodings&);

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
 Lodings(const QDomElement& element);
 
 //! default ctor, initialize with no subelement and no attributes
 Lodings();
 //! Copy constructor.
 Lodings(const Lodings&);
 //! dtor
 ~Lodings() override;
 //! element name
 const std::string& elementName()const override;

 //! child element
 DataExtend *dataExtend{nullptr};
 //! child element
 std::vector<LodingName *> lodingName;
};
} // namespace

#endif
