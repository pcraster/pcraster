/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#ifndef INCLUDED_PCRGENXML_CURSOR
#define INCLUDED_PCRGENXML_CURSOR

#include "pcrxml_positiveinteger.h"
#include "pcrxml_element.h"

#include <string>



class QDomNode;
namespace pcrxml {
 //! Do not edit, generated from PCRaster dtd/schema pcrxml::Element has detailed documentation
class Cursor : public Element {
private:

 //! clean up
 void clean();
 //! name of the element
 static const std::string d_elementName;

 Cursor& operator=(const Cursor&);

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
 Cursor(const QDomElement& element);
 
 //! default ctor, initialize with no subelement and no attributes
 Cursor();
 //! Copy constructor.
 Cursor(const Cursor&);
 //! dtor
 ~Cursor() override;
 //! element name
 const std::string& elementName()const override;

 //! attribute
 PositiveInteger x;
 //! attribute
 PositiveInteger y;
 //! attribute
 PositiveInteger t;
};
} // namespace

#endif
