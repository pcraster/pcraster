/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#ifndef INCLUDED_PCRGENXML_DATAENVELOP
#define INCLUDED_PCRGENXML_DATAENVELOP


#ifndef INCLUDED_PCRGENXML_DATAENVELOPENCODING
#include "pcrgenxml_dataenvelopencoding.h"
#define INCLUDED_PCRGENXML_DATAENVELOPENCODING
#endif



#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif



#ifndef INCLUDED_PCRXML_PCDATAELEMENT
#include "pcrxml_pcdataelement.h"
#define INCLUDED_PCRXML_PCDATAELEMENT
#endif


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
  void       fill(QDomElement el) const;
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
 ~DataEnvelop();
 //! element name
 const std::string& elementName()const;

 //! attribute
 DataEnvelopEncoding encoding;
};
} // namespace

#endif
