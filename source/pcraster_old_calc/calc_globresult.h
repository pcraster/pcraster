#ifndef INCLUDED_CALC_GLOBRESULT
#define INCLUDED_CALC_GLOBRESULT

#include "stddefx.h"
#include "vsenum.h"
#include "calc_field.h"



namespace calc {
  // GlobResult declarations.
}



namespace calc {

class Compressor;
class ApiMap;

//! Result (ouput) of global operations that need to create MAP_* structs
class GlobResult
{

private:

  //! Assignment operator. NOT IMPLEMENTED.
  GlobResult&           operator=           (const GlobResult&);

  //! Copy constructor. NOT IMPLEMENTED.
                     GlobResult             (const GlobResult&);

  const VS          d_concreteVs;
  const Compressor& d_compressor;
  ApiMap*           d_apiMap;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   GlobResult(VS interfaceVs,
                              VS concreteVs,
                              const Compressor& c);

  /* virtual */    ~GlobResult              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  void detachData();
  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  void*       MAPinterface() const;
  FieldHandle createField() const;

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



} // namespace calc

#endif
