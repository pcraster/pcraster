#ifndef INCLUDED_CALC_EXTERNALSYMBOLS
#define INCLUDED_CALC_EXTERNALSYMBOLS



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.
#ifndef INCLUDED_VECTOR
#include <vector>
#define INCLUDED_VECTOR
#endif

// PCRaster library headers.
#ifndef INCLUDED_PCRCALCD
#include "pcrcalcd.h"
#define INCLUDED_PCRCALCD
#endif

// Module headers.
#ifndef INCLUDED_CALC_MODELLINK
#include "calc_modellink.h"
#define INCLUDED_CALC_MODELLINK
#endif

#ifndef INCLUDED_CALC_OPERATOR
#include "calc_operator.h"
#define INCLUDED_CALC_OPERATOR
#endif



namespace com {
  class DynamicLibrary;
}
namespace calc {
  // ExternalSymbols declarations.
}



namespace calc {

//! short_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
/*!
  longer_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
*/
class ExternalFunction: public Operator
{

private:

  PCR_EXTERNAL_FUNCTION_RESULT_TYPES d_resultTypes;

  PCR_EXTERNAL_FUNCTION_ALGORITHM    d_algorithm;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   ExternalFunction
                             (const Operator& op,
                              PCR_EXTERNAL_FUNCTION_RESULT_TYPES resultTypes,
                              PCR_EXTERNAL_FUNCTION_ALGORITHM algorithm)
    : Operator(op),
      d_resultTypes(resultTypes), d_algorithm(algorithm)
  {};

  /* virtual */    ~ExternalFunction   ()
  {};

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  PCR_EXTERNAL_FUNCTION_RESULT_TYPES ResultTypes() const
  { return d_resultTypes; }

  PCR_EXTERNAL_FUNCTION_ALGORITHM Algorithm() const
  { return d_algorithm; }

};



//! short_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
/*!
  longer_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
*/
class ModelLinkProxy
{

private:

  //! Name of the model link.
  std::string      d_name;

  //! Function to create ModelLink object.
  PCR_EXTERNAL_MODELLINK_CREATOR d_creator;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   ModelLinkProxy      (const std::string& name,
                                        PCR_EXTERNAL_MODELLINK_CREATOR creator);

  /* virtual */    ~ModelLinkProxy     ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  const std::string& name              () const;

  const PCR_EXTERNAL_MODELLINK_CREATOR creator() const;

};



//! The ExternalSymbols class is for loading symbols from shared libraries.
/*!
  After loading symbols from shared libraries you can query the singleton
  ExternSymbols instance for functions and/or modellinks.

  \todo Most member functions should be static? Or abbandon the singleton idea?
        Is it useful to have more than one ExternalSymbols object in one
        program?
*/
class ExternalSymbols
{

private:

  // Unique, static, instance.
  static ExternalSymbols* d_instance;

  //! External functions read.
  std::vector<ExternalFunction> d_table;

  //! Proxies to provide access to the ModelLink creator functions.
  std::vector<ModelLinkProxy> d_modelLinkProxies;

  //! Loaded libraries.
  std::vector<com::DynamicLibrary *> d_libraries;

  //! Assignment operator. NOT IMPLEMENTED.
  ExternalSymbols& operator=           (const ExternalSymbols&);

  //! Copy constructor. NOT IMPLEMENTED.
                   ExternalSymbols     (const ExternalSymbols&);

  void             addFunction
                             (const PCR_EXTERNAL_FUNCTION_SYNOPSIS& synopsis);

  void             addModelLink
                             (const PCR_EXTERNAL_MODELLINK_SYNOPSIS& synopsis);

protected:

                   ExternalSymbols     ();

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

  /* virtual */    ~ExternalSymbols    ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  static ExternalSymbols* instance     ();

  static void      clear               ();

  const Operator * find                (const std::string& name) const;

  const ModelLinkProxy* findModelLinkProxy
                                       (const std::string& name) const;

  const Operator&  op                  (size_t t)
  { PRECOND(t < d_table.size());
    return d_table[t]; };

  const ExternalFunction& externFunc   (size_t t)
  { PRECOND(t < d_table.size());
    return d_table[t]; };

  size_t           nrModelLinkProxies  () const;

  size_t           nrExternalFunctions () const;

  const ModelLinkProxy& modelLinkProxy (size_t i) const;

  void             loadDynamicLibrary  (const std::string& extLibNoExt);

  size_t           nrLibrariesLoaded   () const;

};



//------------------------------------------------------------------------------
// INLINE FUNCIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace calc

#endif
