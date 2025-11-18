/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#ifndef INCLUDED_PCRGENXML_FILEINPUT
#define INCLUDED_PCRGENXML_FILEINPUT

#include "pcrgenxml_data.h"
#include "pcrgenxml_map.h"
#include "pcrgenxml_stack.h"
#include "pcrgenxml_table.h"
#include "pcrgenxml_timeseries.h"
#include "pcrxml_boolean.h"
#include "pcrxml_element.h"

#include <string>
#include <vector>



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
  void       fill(QDomElement el) const override;
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
 ~FileInput() override;
 //! element name
 const std::string& elementName()const override;

 //! attribute
 Boolean canChooseOtherFiles;
 //! child element
 Map *map{nullptr};
 //! child element
 Stack *stack{nullptr};
 //! child element
 TimeSeries *timeSeries{nullptr};
 //! child element
 Table *table{nullptr};
 //! child element
 std::vector<Data *> data;
};
} // namespace

#endif
