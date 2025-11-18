/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#ifndef INCLUDED_PCRGENXML_INPUT
#define INCLUDED_PCRGENXML_INPUT

#include "pcrgenxml_inputfile.h"
#include "pcrgenxml_inputlodings.h"
#include "pcrgenxml_inputpoints.h"
#include "pcrgenxml_migrationdirection.h"
#include "pcrxml_boolean.h"
#include "pcrxml_double.h"
#include "pcrxml_element.h"

#include <string>
#include <vector>



class QDomNode;
namespace pcrxml {
 //! Do not edit, generated from PCRaster dtd/schema pcrxml::Element has detailed documentation
class Input : public Element {
private:

 //! clean up
 void clean();
 //! name of the element
 static const std::string d_elementName;

 Input& operator=(const Input&);

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
 Input(const QDomElement& element);
 
 //! default ctor, initialize with no subelement and no attributes
 Input();
 //! Copy constructor.
 Input(const Input&);
 //! dtor
 ~Input() override;
 //! element name
 const std::string& elementName()const override;

 //! attribute
 Boolean flipZ;
 //! attribute
 Double samplingInterval;
 //! attribute
 MigrationDirection migrDirection;
 //! child element
 InputLodings *inputLodings{nullptr};
 //! child element
 InputPoints *inputPoints{nullptr};
 //! child element
 std::vector<InputFile *> inputFile;
};
} // namespace

#endif
