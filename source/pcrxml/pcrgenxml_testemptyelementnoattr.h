/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#ifndef INCLUDED_PCRGENXML_TESTEMPTYELEMENTNOATTR
#define INCLUDED_PCRGENXML_TESTEMPTYELEMENTNOATTR

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
class TestEmptyElementNoAttr : public Element {
private:

 //! clean up
 void clean();
 //! name of the element
 static const std::string d_elementName;

 TestEmptyElementNoAttr& operator=(const TestEmptyElementNoAttr&);

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
 TestEmptyElementNoAttr(const QDomElement& element);
 
 //! default ctor, initialize with no subelement and no attributes
 TestEmptyElementNoAttr();
 //! Copy constructor.
 TestEmptyElementNoAttr(const TestEmptyElementNoAttr&);
 //! dtor
 ~TestEmptyElementNoAttr();
 //! element name
 const std::string& elementName()const;

};
} // namespace

#endif
