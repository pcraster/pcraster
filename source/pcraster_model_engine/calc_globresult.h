#ifndef INCLUDED_CALC_GLOBRESULT
#define INCLUDED_CALC_GLOBRESULT



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_CALC_TYPES
#include "calc_types.h"
#define INCLUDED_CALC_TYPES
#endif
#ifndef INCLUDED_CALC_UNPACKEDCREATION
#include "calc_unpackedcreation.h"
#define INCLUDED_CALC_UNPACKEDCREATION
#endif



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
