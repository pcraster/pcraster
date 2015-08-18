/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#ifndef INCLUDED_PCRGENXML_VARIABLE
#define INCLUDED_PCRGENXML_VARIABLE


#ifndef INCLUDED_PCRGENXML_DATATYPEDTD
#include "pcrgenxml_datatypedtd.h"
#define INCLUDED_PCRGENXML_DATATYPEDTD
#endif



#ifndef INCLUDED_PCRGENXML_INPUTTYPE
#include "pcrgenxml_inputtype.h"
#define INCLUDED_PCRGENXML_INPUTTYPE
#endif



#ifndef INCLUDED_PCRGENXML_OUTPUTTYPE
#include "pcrgenxml_outputtype.h"
#define INCLUDED_PCRGENXML_OUTPUTTYPE
#endif



#ifndef INCLUDED_PCRGENXML_SPATIAL
#include "pcrgenxml_spatial.h"
#define INCLUDED_PCRGENXML_SPATIAL
#endif



#ifndef INCLUDED_PCRXML_STRING
#include "pcrxml_string.h"
#define INCLUDED_PCRXML_STRING
#endif



#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif



#ifndef INCLUDED_VECTOR
#include <vector>
#define INCLUDED_VECTOR
#endif



#ifndef INCLUDED_PCRXML_ELEMENT
#include "pcrxml_element.h"
#define INCLUDED_PCRXML_ELEMENT
#endif


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
  void       fill(QDomElement el) const;
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
 ~Variable();
 //! element name
 const std::string& elementName()const;

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
