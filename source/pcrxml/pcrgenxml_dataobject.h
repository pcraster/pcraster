/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#ifndef INCLUDED_PCRGENXML_DATAOBJECT
#define INCLUDED_PCRGENXML_DATAOBJECT

#include "pcrgenxml_cursor.h"
#include "pcrgenxml_data.h"
#include "pcrxml_element.h"

#include <string>
#include <vector>



class QDomNode;
namespace pcrxml {
 //! Do not edit, generated from PCRaster dtd/schema pcrxml::Element has detailed documentation
class DataObject : public Element {
private:

 //! clean up
 void clean();
 //! name of the element
 static const std::string d_elementName;

 DataObject& operator=(const DataObject&);

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
 DataObject(const QDomElement& element);
 
 //! default ctor, initialize with no subelement and no attributes
 DataObject();
 //! Copy constructor.
 DataObject(const DataObject&);
 //! dtor
 ~DataObject() override;
 //! element name
 const std::string& elementName()const override;

 //! child element
 Cursor *cursor{nullptr};
 //! child element
 std::vector<Data *> data;
};
} // namespace

#endif
