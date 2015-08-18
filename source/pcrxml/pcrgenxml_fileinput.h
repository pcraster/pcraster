/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#ifndef INCLUDED_PCRGENXML_FILEINPUT
#define INCLUDED_PCRGENXML_FILEINPUT


#ifndef INCLUDED_PCRGENXML_DATA
#include "pcrgenxml_data.h"
#define INCLUDED_PCRGENXML_DATA
#endif



#ifndef INCLUDED_PCRGENXML_MAP
#include "pcrgenxml_map.h"
#define INCLUDED_PCRGENXML_MAP
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



#ifndef INCLUDED_PCRXML_BOOLEAN
#include "pcrxml_boolean.h"
#define INCLUDED_PCRXML_BOOLEAN
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
class FileInput : public Element {
private:

 //! clean up
 void clean();
 //! name of the element
 static const std::string d_elementName;

 FileInput& operator=(const FileInput&);

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
 FileInput(const QDomElement& element);
 
 //! default ctor, initialize with no subelement and no attributes
 FileInput();
 //! Copy constructor.
 FileInput(const FileInput&);
 //! dtor
 ~FileInput();
 //! element name
 const std::string& elementName()const;

 //! attribute
 Boolean canChooseOtherFiles;
 //! child element
 Map *map;
 //! child element
 Stack *stack;
 //! child element
 TimeSeries *timeSeries;
 //! child element
 Table *table;
 //! child element
 std::vector<Data *> data;
};
} // namespace

#endif
