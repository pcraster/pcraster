/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#ifndef INCLUDED_PCRGENXML_INPUTLODINGS
#define INCLUDED_PCRGENXML_INPUTLODINGS


#ifndef INCLUDED_PCRGENXML_LODINGINPUTTYPE
#include "pcrgenxml_lodinginputtype.h"
#define INCLUDED_PCRGENXML_LODINGINPUTTYPE
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
class InputLodings : public Element {
private:

 //! clean up
 void clean();
 //! name of the element
 static const std::string d_elementName;

 InputLodings& operator=(const InputLodings&);

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
 InputLodings(const QDomElement& element);
 
 //! default ctor, initialize with no subelement and no attributes
 InputLodings();
 //! Copy constructor.
 InputLodings(const InputLodings&);
 //! dtor
 ~InputLodings();
 //! element name
 const std::string& elementName()const;

 //! attribute
 LodingInputType inputType;
};
} // namespace

#endif
