#ifndef INCLUDED_OLDCALC_GLOBARG
#define INCLUDED_OLDCALC_GLOBARG

#include "stddefx.h"
#include "vsenum.h"
#include "calc_field.h"
#include "calc_decompresseddata.h"



namespace calc {
  // GlobArg declarations.
}



namespace calc {

class Compressor;
class ApiMap;

//! Result (ouput) of global operations that need to create MAP_* structs
class GlobArg
{

private:

  //! Assignment operator. NOT IMPLEMENTED.
  GlobArg&           operator=           (const GlobArg&);

  //! Copy constructor. NOT IMPLEMENTED.
                     GlobArg             (const GlobArg&);

  DecompressedData  d_copy;
  ApiMap*           d_apiMap;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   GlobArg(VS interfaceVs,
                           const FieldHandle& field,
                           const Compressor& c);

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
