#ifndef INCLUDED_CALC_INPUTSPATIAL
#define INCLUDED_CALC_INPUTSPATIAL

#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_CALC_SPATIAL
#include "calc_spatial.h"
#define INCLUDED_CALC_SPATIAL
#endif
#ifndef INCLUDED_CALC_GRIDMAP
#include "calc_gridmap.h"
#define INCLUDED_CALC_GRIDMAP
#endif
#ifndef INCLUDED_CALC_COMPRESSOR
#include "calc_compressor.h"
#define INCLUDED_CALC_COMPRESSOR
#endif



namespace calc {
  // InputSpatial declarations.
}



namespace calc {



//! Implements a loadExternal to allow the load of file based data
/*!
   The file format is the template argument.
*/
template<typename MapFormat>
 class InputSpatial : public Spatial
{

private:

  //! Assignment operator. NOT IMPLEMENTED.
  InputSpatial&           operator=           (const InputSpatial&);

  //! Copy constructor. NOT IMPLEMENTED.
                   InputSpatial               (const InputSpatial&);
  //! illegal
  InputSpatial();

  //! filename of map
  const std::string& d_mapName;

  const Compressor&  d_compressor;

  void loadExternal() const
  {
   MapFormat m(d_mapName);
   loadGrid(m,d_compressor);
  }


public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------
  //! ctor
  InputSpatial(const std::string& mapName,VS vs,const Compressor& c):
    Spatial(vs,c.nrCellsCompressed(),false),
    d_mapName(mapName), d_compressor(c) {};

  //! dtor
  virtual     ~InputSpatial              () {};

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

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
