// #ifndef INCLUDED_STDDEFX
// #include "stddefx.h"
// #define INCLUDED_STDDEFX
// #endif

#ifndef INCLUDED_PCRXSD_UTILS
#include "pcrxsd_utils.h"
#define INCLUDED_PCRXSD_UTILS
#endif

// Library headers.
#ifndef INCLUDED_CCTYPE
#include <cctype>
#define INCLUDED_CCTYPE
#endif
#include <xercesc/dom/DOMDocument.hpp>
#include <xercesc/dom/DOMElement.hpp>
#include <xercesc/util/XMLString.hpp>
XERCES_CPP_NAMESPACE_USE
// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_PCRXSD_DOMINPUT
#include "pcrxsd_dominput.h"
#define INCLUDED_PCRXSD_DOMINPUT
#endif


/*!
  \file
  This file contains the implementation of the utils class.
*/



namespace pcrxsd {

//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

/*
 * \todo
 *  when recognizing script or xml does this:
 *  <pre>
 *   if first non white space character is &lt;
 *    then XML (valid or not), otherwise a script and check for
 *    #! on first position
 *   Seems to do it for all our formats, except table:
 *   &lt; , 8> or &lt; 4 , ]
 *   table does not contain any letter characters while xml
 *   should (element names)
 *  </pre>
 *
 *  \todo very weak test, non well formed "almost xml not recognized"
 *  \returns empty string if not xml, the document element otherwise
 *  \exception pcrxsd::Exception if not well formed
 */
std::string contentsIsXMLOrPCRasterFileFormat(std::string const& contents)
{
  bool firstNonWhiteSpaceCharIsLt=false;
  size_t i=0;
  for( ;i < contents.size(); ++i)
    if(!std::isspace(contents[i])) {
     firstNonWhiteSpaceCharIsLt= contents[i]=='<';
     break;
    }
  if (!firstNonWhiteSpaceCharIsLt)
    return std::string();
  bool hasAlpha=false;
  for( ;i < contents.size() && !hasAlpha; ++i)
    if(std::isalpha(contents[i]))
      hasAlpha=true;
  if (hasAlpha) {
      DOMInput dom;
      dom.setValidate(false);
      dom.setString(contents);
      try {
        DOMDocument* doc=dom.document();
        return toString(doc->getDocumentElement()->getTagName());
      } catch(Exception &) {
        //PRINT_VAR(contents);
      }
  }
  return std::string();
}

//! transcode XMLCh data to local code page for std::string
std::string toString(const XMLCh* const toTranscode) {
    char*   fLocalForm = XMLString::transcode(toTranscode);
    std::string str(fLocalForm);
    XMLString::release(&fLocalForm);
    return str;
}

} // namespace pcrxsd
