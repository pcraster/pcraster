/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#ifndef INCLUDED_PCRGENXML_BINDING
#define INCLUDED_PCRGENXML_BINDING


#ifndef INCLUDED_PCRGENXML_MAP
#include "pcrgenxml_map.h"
#define INCLUDED_PCRGENXML_MAP
#endif



#ifndef INCLUDED_PCRGENXML_NONSPATIAL
#include "pcrgenxml_nonspatial.h"
#define INCLUDED_PCRGENXML_NONSPATIAL
#endif



#ifndef INCLUDED_PCRGENXML_STACK
#include "pcrgenxml_stack.h"
#define INCLUDED_PCRGENXML_STACK
#endif



#ifndef INCLUDED_PCRGENXML_TABLE
#include "pcrgenxml_table.h"
#define INCLUDED_PCRGENXML_TABLE
#endif



#ifndef INCLUDED_PCRGENXML_TIMESERIES
#include "pcrgenxml_timeseries.h"
#define INCLUDED_PCRGENXML_TIMESERIES
#endif



#ifndef INCLUDED_PCRXML_STRING
#include "pcrxml_string.h"
#define INCLUDED_PCRXML_STRING
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
class Binding : public Element {
private:

 //! clean up
 void clean();
 //! name of the element
 static const std::string d_elementName;

 Binding& operator=(const Binding&);

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
 Binding(const QDomElement& element);
 
 //! default ctor, initialize with no subelement and no attributes
 Binding();
 //! Copy constructor.
 Binding(const Binding&);
 //! dtor
 ~Binding();
 //! element name
 const std::string& elementName()const;

 //! attribute
 String parameter;
 //! attribute
 String value;
 //! child element
 Map *map;
 //! child element
 NonSpatial *nonSpatial;
 //! child element
 Stack *stack;
 //! child element
 TimeSeries *timeSeries;
 //! child element
 Table *table;
};
} // namespace

#endif
