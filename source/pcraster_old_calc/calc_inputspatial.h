#ifndef INCLUDED_OLDCALC_INPUTSPATIAL
#define INCLUDED_OLDCALC_INPUTSPATIAL

#include "stddefx.h"
#include "calc_spatial.h"
#include "calc_gridmap.h"
#include "calc_compressor.h"



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

  void loadExternal() const override
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
    d_mapName(mapName), d_compressor(c) {}

  //! dtor
      ~InputSpatial              () override {}

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
