/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#ifndef INCLUDED_PCRGENXML_DATAENVELOP
#define INCLUDED_PCRGENXML_DATAENVELOP

#include "pcrgenxml_dataenvelopencoding.h"
#include "pcrxml_pcdataelement.h"

#include <string>

class QDomNode;
namespace pcrxml {
 //! Do not edit, generated from PCRaster dtd/schema pcrxml::Element has detailed documentation
class DataEnvelop : public PCDATAElement {
private:

 //! clean up
 void clean();
 //! name of the element
 static const std::string d_elementName;

 DataEnvelop& operator=(const DataEnvelop&);

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
 DataEnvelop(const QDomElement& element);
 
      //! ctor with text
      DataEnvelop(const std::string& content);
    
 //! default ctor, initialize with no subelement and no attributes
 DataEnvelop();
 //! Copy constructor.
 DataEnvelop(const DataEnvelop&);
 //! dtor
 ~DataEnvelop() override;
 //! element name
 const std::string& elementName()const override;

 //! attribute
 DataEnvelopEncoding encoding;
};
} // namespace

#endif
