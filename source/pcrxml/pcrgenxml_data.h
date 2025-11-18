/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#ifndef INCLUDED_PCRGENXML_DATA
#define INCLUDED_PCRGENXML_DATA

#include "pcrgenxml_iotype.h"
#include "pcrgenxml_map.h"
#include "pcrgenxml_nonspatial.h"
#include "pcrgenxml_stack.h"
#include "pcrgenxml_table.h"
#include "pcrgenxml_timeseries.h"
#include "pcrxml_string.h"
#include "pcrxml_element.h"

#include <string>


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
  void       fill(QDomElement el) const override;
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
 ~Data() override;
 //! element name
 const std::string& elementName()const override;

 //! attribute
 String name;
 //! attribute
 String description;
 //! attribute
 String externalFileName;
 //! attribute
 IoType ioType;
 //! child element
 Map *map{nullptr};
 //! child element
 NonSpatial *nonSpatial{nullptr};
 //! child element
 Stack *stack{nullptr};
 //! child element
 TimeSeries *timeSeries{nullptr};
 //! child element
 Table *table{nullptr};
};
} // namespace

#endif
