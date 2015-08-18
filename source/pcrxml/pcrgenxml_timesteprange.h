/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#ifndef INCLUDED_PCRGENXML_TIMESTEPRANGE
#define INCLUDED_PCRGENXML_TIMESTEPRANGE


#ifndef INCLUDED_PCRXML_POSITIVEINTEGER
#include "pcrxml_positiveinteger.h"
#define INCLUDED_PCRXML_POSITIVEINTEGER
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
class TimestepRange : public Element {
private:

 //! clean up
 void clean();
 //! name of the element
 static const std::string d_elementName;

 TimestepRange& operator=(const TimestepRange&);

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
 TimestepRange(const QDomElement& element);
 
 //! default ctor, initialize with no subelement and no attributes
 TimestepRange();
 //! Copy constructor.
 TimestepRange(const TimestepRange&);
 //! dtor
 ~TimestepRange();
 //! element name
 const std::string& elementName()const;

 //! attribute
 PositiveInteger first;
 //! attribute
 PositiveInteger last;
};
} // namespace

#endif
