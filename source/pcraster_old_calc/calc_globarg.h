#ifndef INCLUDED_CALC_GLOBARG
#define INCLUDED_CALC_GLOBARG



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_VSENUM
#include "vsenum.h"
#define INCLUDED_VSENUM
#endif
#ifndef INCLUDED_CALC_FIELD
#include "calc_field.h"
#define INCLUDED_CALC_FIELD
#endif
#ifndef INCLUDED_CALC_DECOMPRESSEDDATA
#include "calc_decompresseddata.h"
#define INCLUDED_CALC_DECOMPRESSEDDATA
#endif



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
                           const FieldHandle field,
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
