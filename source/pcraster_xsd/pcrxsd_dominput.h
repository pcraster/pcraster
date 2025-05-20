#ifndef INCLUDED_PCRXSD_DOMINPUT
#define INCLUDED_PCRXSD_DOMINPUT

// #ifndef INCLUDED_STDDEFX
// #include "stddefx.h"
// #define INCLUDED_STDDEFX
// #endif

// Library headers.
// XERCES_CPP_NAMESPACE
#include <xercesc/util/XercesVersion.hpp>
// PCRaster library headers.

// Module headers.

#include <string>


namespace XERCES_CPP_NAMESPACE {
  // DOMInput declarations.
  class DOMLSParser;
  class DOMDocument;
  class DOMLSResourceResolver;
}



namespace pcrxsd {

class Exception {
  std::string d_msg;
public:
  Exception(std::string const& msg):
    d_msg(msg)
  {
  }
  std::string const& msg() const {
    return d_msg;
  }
};

//! Input a DOM document with the option to to use the SupportedSchema collection
/*!
   Read a DOM document specified by setFile() or setString() under
   the conditions of setValidate() (default false)

   Two types error handlers can be specified, see DOMInput::ErrorHandlerType

*/
class DOMInput
{
public:
  //! type of error handler
  enum ErrorHandlerType {
    //! format readable by vi(m)
    Vi,
    //! verbose human readable
    Verbose
  };
  //! type of error handler
  enum EntityResolverType {
    //! The default xerces mechanism
    DefaultEntityResolver,
    //! Resolve by the compiled in schema's (for Aguila and pcrme)
    CompiledIn
    // //! Development, expect environment variable PCRTREE set, and look in $PCRTREE/template/xml
    // PCRTREE
  };

private:

  friend class DOMInputTest;

  XERCES_CPP_NAMESPACE::DOMLSParser *d_parser;
  XERCES_CPP_NAMESPACE::DOMDocument *d_document;
  //! stores PCRaster.xsd
  XERCES_CPP_NAMESPACE::DOMLSResourceResolver *d_resourceResolver;

  ErrorHandlerType           d_errorHandlerType{Verbose};


  //! default false, check wellFormed only
  bool             d_validate{false};

  //! set either d_file or d_string not both
  std::string      d_file;
  //! set either d_file or d_string not both
  std::string      d_string;

  void             clearBuilder           ();

protected:

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   DOMInput               (const DOMInput&) = delete;

  DOMInput&        operator=              (const DOMInput&) = delete;

                   DOMInput               (EntityResolverType erType=DefaultEntityResolver);

  virtual         ~DOMInput               ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
  void             setErrorHandlerType (ErrorHandlerType errorHandlerType);
  void             setValidate         (bool validate);
  void             setFile             (const std::string& file);
  void             setString           (const std::string& string);

  XERCES_CPP_NAMESPACE::DOMDocument* document       ();

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  ErrorHandlerType   errorHandlerType  () const;
  bool               validate          () const;
  const std::string& file              () const;
  const std::string& string            () const;

};



//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace pcrxsd

#endif
