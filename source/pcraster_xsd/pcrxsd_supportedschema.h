#ifndef INCLUDED_PCRXSD_SUPPORTEDSCHEMA
#define INCLUDED_PCRXSD_SUPPORTEDSCHEMA

#include <xercesc/util/XercesVersion.hpp>

#include <string>



namespace XERCES_CPP_NAMESPACE {
  class DOMLSInput;
}


namespace pcrxsd {

//! Schema supported by pcrxsd
/*!
   pcrxsd explictly support a number of schema files (entities) residing
   in $PCRTREE/template/xml. For each such schema a SupportedSchema object
   exists.

   \warning SupportedSchemaMap will create SupportedSchema object in
   a static structure, therefor all members of SupportedSchema should
   have constructors who do NOT depend on significant runtime
   initialization.
*/
class SupportedSchema
{

  friend class SupportedSchemaTest;

  friend class SupportedSchemaMap;

private:

  //! Assignment operator. NOT IMPLEMENTED.
  SupportedSchema&           operator=           (SupportedSchema const& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
  //               SupportedSchema               (SupportedSchema const& rhs);
  //! Default constructor. NOT IMPLEMENTED.
  //               SupportedSchema               ();


   const char       *d_contents;
   std::string       d_systemId;
   const char       *d_tag;

protected:

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   SupportedSchema               (const char  *contents,
                                                  const std::string&  systemId,
                                                  const char  *tag);

  /* virtual */    ~SupportedSchema              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  XERCES_CPP_NAMESPACE::DOMLSInput* createInputSource() const;

  static SupportedSchema const*         findBySystemId   (std::string const& systemId);
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


} // namespace

#endif
