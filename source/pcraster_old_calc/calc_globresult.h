#ifndef INCLUDED_CALC_GLOBRESULT
#define INCLUDED_CALC_GLOBRESULT



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
