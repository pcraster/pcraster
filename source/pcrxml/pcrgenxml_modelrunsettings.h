/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#ifndef INCLUDED_PCRGENXML_MODELRUNSETTINGS
#define INCLUDED_PCRGENXML_MODELRUNSETTINGS


#ifndef INCLUDED_PCRGENXML_BINDING
#include "pcrgenxml_binding.h"
#define INCLUDED_PCRGENXML_BINDING
#endif



#ifndef INCLUDED_PCRGENXML_FILESETTING
#include "pcrgenxml_filesetting.h"
#define INCLUDED_PCRGENXML_FILESETTING
#endif



#ifndef INCLUDED_PCRGENXML_MODELRUNCHILD
#include "pcrgenxml_modelrunchild.h"
#define INCLUDED_PCRGENXML_MODELRUNCHILD
#endif



#ifndef INCLUDED_PCRGENXML_NUMERICSETTING
#include "pcrgenxml_numericsetting.h"
#define INCLUDED_PCRGENXML_NUMERICSETTING
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
class ModelRunSettings : public Element {
private:

 //! clean up
 void clean();
 //! name of the element
 static const std::string d_elementName;

 ModelRunSettings& operator=(const ModelRunSettings&);

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
 ModelRunSettings(const QDomElement& element);
 
 //! default ctor, initialize with no subelement and no attributes
 ModelRunSettings();
 //! Copy constructor.
 ModelRunSettings(const ModelRunSettings&);
 //! dtor
 ~ModelRunSettings();
 //! element name
 const std::string& elementName()const;

 //! child element
 std::vector<NumericSetting *> numericSetting;
 //! child element
 std::vector<FileSetting *> fileSetting;
 //! child element
 std::vector<Binding *> binding;
 //! child element
 std::vector<ModelRunChild *> modelRunChild;
};
} // namespace

#endif
