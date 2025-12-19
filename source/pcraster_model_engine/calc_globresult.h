#ifndef INCLUDED_CALC_GLOBRESULT
#define INCLUDED_CALC_GLOBRESULT

#include "stddefx.h"
#include "calc_types.h"
#include "calc_unpackedcreation.h"



namespace calc {
  // GlobResult declarations.
}



namespace calc {

class SpatialPacking;
class ApiMap;
class Field;

//! Result (ouput) of global operations that need to create MAP_* structs
class GlobResult
{

private:

  //! Assignment operator. NOT IMPLEMENTED.
  GlobResult&           operator=           (const GlobResult&);

  //! Copy constructor. NOT IMPLEMENTED.
                     GlobResult             (const GlobResult&);

  UnpackedCreation  d_up;
  ApiMap*           d_apiMap;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   GlobResult(VS interfaceVs,
                              VS concreteVs,
                              const SpatialPacking& c);

  /* virtual */    ~GlobResult              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  Field*      createField();
  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  void*       MAPinterface() const;

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
