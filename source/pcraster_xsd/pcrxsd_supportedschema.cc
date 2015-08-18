// #ifndef INCLUDED_STDDEFX
// #include "stddefx.h"
// #define INCLUDED_STDDEFX
// #endif

#ifndef INCLUDED_PCRXSD_SUPPORTEDSCHEMA
#include "pcrxsd_supportedschema.h"
#define INCLUDED_PCRXSD_SUPPORTEDSCHEMA
#endif

// Library headers.
#ifndef INCLUDED_BOOST_FILESYSTEM_OPERATIONS
#include "boost/filesystem/operations.hpp"
#define INCLUDED_BOOST_FILESYSTEM_OPERATIONS
#endif

#ifndef INCLUDED_MAP
#include <map>
#define INCLUDED_MAP
#endif
#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif
#include <xercesc/framework/MemBufInputSource.hpp>
#include <xercesc/framework/LocalFileInputSource.hpp>
#include <xercesc/framework/Wrapper4InputSource.hpp>
#include <xercesc/util/XMLString.hpp>
XERCES_CPP_NAMESPACE_USE

// PCRaster library headers.

// Module headers.
#ifdef PCRTREE2_BUILD
# include "compiled_PCRaster.h"
#endif
#ifdef AGUILA_BUILD
# include "compiled_Aguila.h"
#endif
#include "compiled_commonTypes.h"



/*!
  \file
  This file contains the implementation of the SupportedSchema class.
*/



namespace pcrxsd {

//------------------------------------------------------------------------------

class SupportedSchemaMap :
  public std::map<std::string,SupportedSchema>
{
public:

  SupportedSchemaMap()
  {
   insert(std::make_pair("commonTypes.xsd",
    SupportedSchema(compiled_commonTypes,"commonTypes.xsd", "commonTypes")));
#ifdef AGUILA_BUILD
   insert(std::make_pair("Aguila.xsd",
     SupportedSchema(compiled_Aguila,   "Aguila.xsd",      "Aguila")));
#endif
#ifdef PCRTREE2_BUILD
   insert(std::make_pair("PCRaster.xsd",
     SupportedSchema(compiled_PCRaster, "PCRaster.xsd",    "PCRaster")));
   // for unit test purposes:
   insert(std::make_pair("PCRaster_X_X_X.xsd",
     SupportedSchema(compiled_PCRaster, "PCRaster_X_X_X.xsd","PCRaster")));
#endif
  }

  ~SupportedSchemaMap()
  {
  }
  SupportedSchema const* findBySystemId(
    std::string const& systemId)
  {
   const_iterator f=find(systemId);
   if (f == end())
     return 0;
   return &(f->second);
  }
};




//------------------------------------------------------------------------------
// DEFINITION OF STATIC SUPPORTEDSCHEMA MEMBERS
//------------------------------------------------------------------------------

static SupportedSchemaMap supportedSchemas;

SupportedSchema const* SupportedSchema::findBySystemId(
    std::string const& systemId)
{
  return pcrxsd::supportedSchemas.findBySystemId(systemId);
}

//------------------------------------------------------------------------------
// DEFINITION OF SUPPORTEDSCHEMA MEMBERS
//------------------------------------------------------------------------------

SupportedSchema::SupportedSchema(
   const char       *contents,
   std::string       systemId,
   const char       *tag):
  d_contents(contents),
  d_systemId(systemId),
  d_tag(tag)
{
  assert(contents);
  assert(systemId.size());
  assert(tag);
}



/* NOT IMPLEMENTED
//! Copy constructor.
SupportedSchema::SupportedSchema(
         SupportedSchema const& rhs)

  : Base(rhs)

{
}
*/



SupportedSchema::~SupportedSchema()
{
}

DOMLSInput *SupportedSchema::createInputSource() const
{
    MemBufInputSource* memBufIS = new MemBufInputSource(
       (const XMLByte*)d_contents, strlen(d_contents)+1, d_tag, false);
    return new  Wrapper4InputSource(memBufIS);
}

/* NOT IMPLEMENTED
//! Assignment operator.
SupportedSchema& SupportedSchema::operator=(
         SupportedSchema const& rhs)
{
  if(this != &rhs) {
  }

  return *this;
}
*/



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

} // namespace pcrxsd
