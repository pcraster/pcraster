/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#ifndef INCLUDED_PCRGENXML_DATA
#define INCLUDED_PCRGENXML_DATA


#ifndef INCLUDED_PCRGENXML_IOTYPE
#include "pcrgenxml_iotype.h"
#define INCLUDED_PCRGENXML_IOTYPE
#endif



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
class Data : public Element {
private:

 //! clean up
 void clean();
 //! name of the element
 static const std::string d_elementName;

 Data& operator=(const Data&);

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
 Data(const QDomElement& element);
 
 //! default ctor, initialize with no subelement and no attributes
 Data();
 //! Copy constructor.
 Data(const Data&);
 //! dtor
 ~Data();
 //! element name
 const std::string& elementName()const;

 //! attribute
 String name;
 //! attribute
 String description;
 //! attribute
 String externalFileName;
 //! attribute
 IoType ioType;
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
