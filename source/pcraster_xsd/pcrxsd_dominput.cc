#include "pcrxsd_dominput.h"
#include "pcrxsd_supportedschema.h"
#include "pcrxsd_utils.h"

#include <xercesc/parsers/AbstractDOMParser.hpp>
#include <xercesc/dom/DOMImplementationLS.hpp>
#include <xercesc/dom/DOMImplementationRegistry.hpp>
#include <xercesc/util/PlatformUtils.hpp>
#include <xercesc/dom/DOMLSParser.hpp>
// #include <xercesc/dom/DOMEntityResolver.hpp>
// #include <xercesc/sax/EntityResolver.hpp>
#include <xercesc/dom/DOMLSResourceResolver.hpp>
#include <xercesc/sax/InputSource.hpp>
#include <xercesc/dom/DOMImplementation.hpp>
#include <xercesc/dom/DOMException.hpp>
#include <xercesc/dom/DOMDocument.hpp>
#include <xercesc/dom/DOMNodeList.hpp>
#include <xercesc/dom/DOMError.hpp>
#include <xercesc/dom/DOMLocator.hpp>
#include <xercesc/dom/DOMNamedNodeMap.hpp>
#include <xercesc/dom/DOMAttr.hpp>
#include <xercesc/dom/DOMErrorHandler.hpp>
#include <xercesc/framework/MemBufInputSource.hpp>
#include <xercesc/framework/Wrapper4InputSource.hpp>

XERCES_CPP_NAMESPACE_USE

#include <map>
#include <string>
#include <cstdlib>
#include <fstream>
#include <iostream>
#include <memory>
#include <sstream>

/*!
  \file
  This file contains the implementation of the DOMInput class.
*/



namespace pcrxsd {

class CompiledInResolver : public DOMLSResourceResolver {

public:
  CompiledInResolver() {
  }

  DOMLSInput* resolveResource(
      const XMLCh* const  /* resourceType*/,
      const XMLCh* const  /* namespaceUri */,
      const XMLCh* const  /* publicId */,
      const XMLCh* const    systemId,
      const XMLCh* const  /*  baseURI */) override
  {
    SupportedSchema const* s=SupportedSchema::findBySystemId(
        toString(systemId));
    if (s) {
     return s->createInputSource();
    }
    return nullptr;
  }
};

// class PCRTREEResolver : public EntityResolver {
//
// public:
//   PCRTREEResolver() {
//   }
//
//   InputSource *resolveEntity(
//       const XMLCh *const, // publicId
//       const XMLCh *const systemId)
//  {
//    return SupportedSchema::findInPCRTREE(toString(systemId));
//  }
// };


//------------------------------------------------------------------------------

/*
class DOMInputPrivate
{
public:

  DOMInputPrivate()
  {
  }

  ~DOMInputPrivate()
  {
  }

};
*/

//!  Simple error handler deriviative to install on parser
class DOMInputErrorHandler : public DOMErrorHandler
{
protected:
    std::ostringstream d_msg;
public:
    DOMInputErrorHandler() {}
    ~DOMInputErrorHandler() override {}

    std::string error() const {
      return d_msg.str();
    }

private :
    //!  Unimplemented constructors and operators
    DOMInputErrorHandler(const DOMInputErrorHandler&);
    void operator=(const DOMInputErrorHandler&);
};

//!  Simple error handler deriviative to install on parser
class VerboseErrorHandler : public DOMInputErrorHandler
{
public:
    VerboseErrorHandler() {}
    ~VerboseErrorHandler() override {}

    //!  Implementation of the DOM ErrorHandler interface
    bool handleError(const DOMError& domError) override;
private :
    //!  Unimplemented constructors and operators
    VerboseErrorHandler(const VerboseErrorHandler&);
    void operator=(const VerboseErrorHandler&);
};

//! Override of the DOM ErrorHandler interface
bool pcrxsd::VerboseErrorHandler::handleError(const DOMError& domError)
{

  if (domError.getSeverity() == DOMError::DOM_SEVERITY_WARNING)
      d_msg << "\nWarning at file ";
  else if (domError.getSeverity() == DOMError::DOM_SEVERITY_ERROR)
      d_msg << "\nError at file ";
  else
      d_msg << "\nFatal Error at file ";

  d_msg << domError.getLocation()->getURI()
       << ", line " << domError.getLocation()->getLineNumber()
       << ", char " << domError.getLocation()->getColumnNumber()
       << "\n  Message: " << domError.getMessage() << '\n';
  return true; // do not stop processing
}

//!  Simple error handler deriviative to install on parser
class ViErrorHandler : public DOMInputErrorHandler
{
public:
    ViErrorHandler() {}
    ~ViErrorHandler() override {}

    //!  Implementation of the DOM ErrorHandler interface
    bool handleError(const DOMError& domError) override;
private :
    //!  Unimplemented constructors and operators
    ViErrorHandler(const VerboseErrorHandler&);
    void operator=(const VerboseErrorHandler&);
};

//! Override of the DOM ErrorHandler interface
bool pcrxsd::ViErrorHandler::handleError(const DOMError& domError)
{


  d_msg << domError.getLocation()->getURI()
       << ":" << domError.getLocation()->getLineNumber()
       << ":" << domError.getLocation()->getColumnNumber()
       << ":";

  if (domError.getSeverity() == DOMError::DOM_SEVERITY_WARNING)
      d_msg << "Warning";
  else if (domError.getSeverity() == DOMError::DOM_SEVERITY_ERROR)
      d_msg << "Error";
  else
      d_msg << "Fatal Error";

  d_msg << ": " << domError.getMessage() << '\n';
  return true; //  do not stop processing
}

//------------------------------------------------------------------------------
// DEFINITION OF STATIC DOMINPUT MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF DOMINPUT MEMBERS
//------------------------------------------------------------------------------
/*! \brief ctor for a DOMInput source
 *
 * \param erType how to resolve external entities like DTD's and Schema's
 *
 */
DOMInput::DOMInput(EntityResolverType erType):
  d_parser(nullptr),
  d_document(nullptr),
  d_resourceResolver(nullptr)

{
  switch(erType) {
    case CompiledIn:  d_resourceResolver = new CompiledInResolver(); break;
 // case PCRTREE:  d_resourceResolver = new PCRTREEResolver(); break;
    case DefaultEntityResolver: d_resourceResolver=nullptr; break;
  }
}



/* NOT IMPLEMENTED
//! Copy constructor.
DOMInput::DOMInput(
         DOMInput const& rhs)

  : Base(rhs)

{
}
*/



DOMInput::~DOMInput()
{
  clearBuilder();
  delete d_resourceResolver;
}

//! clear the parsed document
void DOMInput::clearBuilder()
{
  if (d_parser)
   d_parser->release();
  d_parser=nullptr;
  d_document=nullptr;
}



/* NOT IMPLEMENTED
//! Assignment operator.
DOMInput& DOMInput::operator=(
         DOMInput const& rhs)
{
  if(this != &rhs) {
  }

  return *this;
}
*/

//! set value of d_validate
void DOMInput::setValidate(bool validate)
{
  d_validate=validate;
}

//! set value of d_file
void DOMInput::setFile(const std::string& file)
{
  d_file=file;
}

//! set value of d_string
void DOMInput::setString(const std::string& string)
{
  d_string=string;
}

//! get value of d_validate
bool DOMInput::validate() const
{
  return d_validate;
}

//! get value of d_file
const std::string& DOMInput::file() const
{
  return d_file;
}

//! get value of d_string
const std::string& DOMInput::string() const
{
  return d_string;
}

//! return the document
/*!
 * returned pointer stays owned by DOMInput, first call to document
 * will parse, next calls return the cached result of the parse
 */
DOMDocument* DOMInput::document()
{
  if (d_document)
    return d_document;

  clearBuilder();

  // Instantiate the DOM parser.
  static const XMLCh gLS[] = { chLatin_L, chLatin_S, chNull };
  DOMImplementation *impl =
    DOMImplementationRegistry::getDOMImplementation(gLS);

  d_parser = ((DOMImplementationLS*)impl)->createLSParser(
                  DOMImplementationLS::MODE_SYNCHRONOUS, nullptr);

  if (d_parser->getDomConfig()->canSetParameter(XMLUni::fgDOMValidateIfSchema, true))
  {
    d_parser->getDomConfig()->setParameter(XMLUni::fgDOMValidateIfSchema, true);
  }
  if (d_parser->getDomConfig()->canSetParameter(XMLUni::fgDOMValidate, true))
  {
    d_parser->getDomConfig()->setParameter(XMLUni::fgDOMValidate, d_validate);
  }

  if (d_parser->getDomConfig()->canSetParameter(XMLUni::fgDOMNamespaces, true))
  {
    d_parser->getDomConfig()->setParameter(XMLUni::fgDOMNamespaces, true);
  }
  if (d_parser->getDomConfig()->canSetParameter(XMLUni::fgXercesSchema, true))
  {
    d_parser->getDomConfig()->setParameter(XMLUni::fgXercesSchema, true);
  }
  if (d_parser->getDomConfig()->canSetParameter(XMLUni::fgXercesSchemaFullChecking, true))
  {
    d_parser->getDomConfig()->setParameter(XMLUni::fgXercesSchemaFullChecking, true);
  }
 // enable datatype normalization - default is off
  if (d_parser->getDomConfig()->canSetParameter(XMLUni::fgDOMDatatypeNormalization, true))
  {
    d_parser->getDomConfig()->setParameter(XMLUni::fgDOMDatatypeNormalization, true);
  }

  // create our error handler and install it
  //  only have to keep live during parse

  std::unique_ptr<DOMInputErrorHandler> errorHandler;
  if (d_errorHandlerType == Vi)
    errorHandler = std::make_unique<ViErrorHandler>();
  if (d_errorHandlerType == Verbose)
    errorHandler = std::make_unique<VerboseErrorHandler>();

  d_parser->getDomConfig()->setParameter(XMLUni::fgDOMErrorHandler,
   errorHandler.get());

  // see http://old.nabble.com/Using-a-local-DTD-td26984671.html
  d_parser->getDomConfig()->setParameter(XMLUni::fgDOMResourceResolver,
            d_resourceResolver);

  try {
    if (!d_string.empty()) {

      auto* memBufIS = new MemBufInputSource(
           (const XMLByte*)d_string.c_str(), d_string.size(),
           "inMemory", false);
      Wrapper4InputSource wrp(memBufIS);
      d_document=d_parser->parse(&wrp);
    } else {
      assert(!d_file.empty());
      d_document=d_parser->parseURI(d_file.c_str());
    }

  } catch (const XMLException& toCatch) {
     std::ostringstream msg;
       msg << "\nError during parsing, exception message is:  \n"
             << toCatch.getMessage() << "\n" << '\n';
     throw Exception(msg.str());
  } catch (const DOMException& toCatch) {
    const unsigned int maxChars = 2047;
    XMLCh errText[maxChars + 1];
    std::ostringstream msg;
    msg << "\nDOM Error during parsing: DOMException code is: "
        << toCatch.code << '\n';
    if (DOMImplementation::
         loadDOMExceptionMsg(toCatch.code, errText, maxChars))
        msg << "Message is: " << errText << '\n';
    throw Exception(msg.str());
  } catch (...) {
    throw Exception("Unexpected exception in DOMInput");
  }
  if (!errorHandler->error().empty())
      throw Exception(errorHandler->error());
  return d_document;
}

//! set value of d_errorHandlerType
void DOMInput::setErrorHandlerType(ErrorHandlerType errorHandlerType)
{
  d_errorHandlerType=errorHandlerType;
}

//! get value of d_errorHandlerType
DOMInput::ErrorHandlerType DOMInput::errorHandlerType() const
{
  return d_errorHandlerType;
}


//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace pcrxsd
