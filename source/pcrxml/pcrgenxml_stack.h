/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#ifndef INCLUDED_PCRGENXML_STACK
#define INCLUDED_PCRGENXML_STACK


#ifndef INCLUDED_PCRGENXML_DATATYPEDTD
#include "pcrgenxml_datatypedtd.h"
#define INCLUDED_PCRGENXML_DATATYPEDTD
#endif



#ifndef INCLUDED_PCRGENXML_TIMESTEPRANGE
#include "pcrgenxml_timesteprange.h"
#define INCLUDED_PCRGENXML_TIMESTEPRANGE
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
class Stack : public Element {
private:

 //! clean up
 void clean();
 //! name of the element
 static const std::string d_elementName;

 Stack& operator=(const Stack&);

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
 Stack(const QDomElement& element);
 
 //! default ctor, initialize with no subelement and no attributes
 Stack();
 //! Copy constructor.
 Stack(const Stack&);
 //! dtor
 ~Stack();
 //! element name
 const std::string& elementName()const;

 //! child element
 DataTypeDTD *dataTypeDTD;
 //! child element
 TimestepRange *timestepRange;
};
} // namespace

#endif
