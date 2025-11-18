/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#ifndef INCLUDED_PCRGENXML_VARIABLE
#define INCLUDED_PCRGENXML_VARIABLE

#include "pcrgenxml_datatypedtd.h"
#include "pcrgenxml_inputtype.h"
#include "pcrgenxml_outputtype.h"
#include "pcrgenxml_spatial.h"
#include "pcrxml_string.h"
#include "pcrxml_element.h"

#include <string>
#include <vector>


class QDomNode;
namespace pcrxml {
 //! Do not edit, generated from PCRaster dtd/schema pcrxml::Element has detailed documentation
class Variable : public Element {
private:

 //! clean up
 void clean();
 //! name of the element
 static const std::string d_elementName;

 Variable& operator=(const Variable&);

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
 Variable(const QDomElement& element);
 
 //! default ctor, initialize with no subelement and no attributes
 Variable();
 //! Copy constructor.
 Variable(const Variable&);
 //! dtor
 ~Variable() override;
 //! element name
 const std::string& elementName()const override;

 //! attribute
 String id;
 //! attribute
 String description;
 //! attribute
 Spatial spatial;
 //! attribute
 InputType input;
 //! attribute
 OutputType output;
 //! child element
 std::vector<DataTypeDTD *> dataTypeDTD;
};
} // namespace

#endif
