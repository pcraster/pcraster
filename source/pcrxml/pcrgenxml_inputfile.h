/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#ifndef INCLUDED_PCRGENXML_INPUTFILE
#define INCLUDED_PCRGENXML_INPUTFILE

#include "pcrxml_string.h"
#include "pcrxml_element.h"

#include <string>



class QDomNode;
namespace pcrxml {
 //! Do not edit, generated from PCRaster dtd/schema pcrxml::Element has detailed documentation
class InputFile : public Element {
private:

 //! clean up
 void clean();
 //! name of the element
 static const std::string d_elementName;

 InputFile& operator=(const InputFile&);

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
 InputFile(const QDomElement& element);
 
 //! default ctor, initialize with no subelement and no attributes
 InputFile();
 //! Copy constructor.
 InputFile(const InputFile&);
 //! dtor
 ~InputFile() override;
 //! element name
 const std::string& elementName()const override;

 //! attribute
 String value;
};
} // namespace

#endif
