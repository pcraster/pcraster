/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#ifndef INCLUDED_PCRGENXML_INPUT
#define INCLUDED_PCRGENXML_INPUT


#ifndef INCLUDED_PCRGENXML_INPUTFILE
#include "pcrgenxml_inputfile.h"
#define INCLUDED_PCRGENXML_INPUTFILE
#endif



#ifndef INCLUDED_PCRGENXML_INPUTLODINGS
#include "pcrgenxml_inputlodings.h"
#define INCLUDED_PCRGENXML_INPUTLODINGS
#endif



#ifndef INCLUDED_PCRGENXML_INPUTPOINTS
#include "pcrgenxml_inputpoints.h"
#define INCLUDED_PCRGENXML_INPUTPOINTS
#endif



#ifndef INCLUDED_PCRGENXML_MIGRATIONDIRECTION
#include "pcrgenxml_migrationdirection.h"
#define INCLUDED_PCRGENXML_MIGRATIONDIRECTION
#endif



#ifndef INCLUDED_PCRXML_BOOLEAN
#include "pcrxml_boolean.h"
#define INCLUDED_PCRXML_BOOLEAN
#endif



#ifndef INCLUDED_PCRXML_DOUBLE
#include "pcrxml_double.h"
#define INCLUDED_PCRXML_DOUBLE
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
class Input : public Element {
private:

 //! clean up
 void clean();
 //! name of the element
 static const std::string d_elementName;

 Input& operator=(const Input&);

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
 Input(const QDomElement& element);
 
 //! default ctor, initialize with no subelement and no attributes
 Input();
 //! Copy constructor.
 Input(const Input&);
 //! dtor
 ~Input();
 //! element name
 const std::string& elementName()const;

 //! attribute
 Boolean flipZ;
 //! attribute
 Double samplingInterval;
 //! attribute
 MigrationDirection migrDirection;
 //! child element
 InputLodings *inputLodings;
 //! child element
 InputPoints *inputPoints;
 //! child element
 std::vector<InputFile *> inputFile;
};
} // namespace

#endif
