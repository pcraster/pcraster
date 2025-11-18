/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#ifndef INCLUDED_PCRGENXML_MODELRUNSETTINGS
#define INCLUDED_PCRGENXML_MODELRUNSETTINGS

#include "pcrgenxml_binding.h"
#include "pcrgenxml_filesetting.h"
#include "pcrgenxml_modelrunchild.h"
#include "pcrgenxml_numericsetting.h"
#include "pcrxml_element.h"

#include <string>
#include <vector>



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
  void       fill(QDomElement el) const override;
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
 ~ModelRunSettings() override;
 //! element name
 const std::string& elementName()const override;

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
