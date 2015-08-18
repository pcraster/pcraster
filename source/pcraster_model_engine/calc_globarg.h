#ifndef INCLUDED_CALC_GLOBARG
#define INCLUDED_CALC_GLOBARG



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
#ifndef INCLUDED_CALC_UNPACKEDSRC
#include "calc_unpackedsrc.h"
#define INCLUDED_CALC_UNPACKEDSRC
#endif


namespace calc {
  // GlobArg declarations.
}



namespace calc {

class SpatialPacking;
class ApiMap;
class Field;

//! input of global operations that need to create MAP_* structs
class GlobArg
{

private:

  //! Assignment operator. NOT IMPLEMENTED.
  GlobArg&           operator=           (const GlobArg&);

  //! Copy constructor. NOT IMPLEMENTED.
                     GlobArg             (const GlobArg&);

  UnpackedSrc       d_unpackedSrc;
  ApiMap*           d_apiMap;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   GlobArg(VS    interfaceVs,
                           const Field& field,
                           const SpatialPacking& c);

  /* virtual */    ~GlobArg              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

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
