#ifndef INCLUDED_MLDD_WEIGHTMAP
#define INCLUDED_MLDD_WEIGHTMAP

#include "stddefx.h"
#include "mldd_edgevertex.h"
#include "geo_simpleraster.h"



namespace mldd {
  // WeightMap declarations.
  class DagRaster;
}



namespace mldd {



//! weight map for mldd
/*!
   Is is possible to model this as the boost::ReadablePropertyMap concept
*/
class WeightMap
{
  const DagRaster&                d_dr;
  const geo::SimpleRaster<REAL4>& d_dem;

private:

  //! Assignment operator. NOT IMPLEMENTED.
  WeightMap&           operator=           (const WeightMap& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   WeightMap               (const WeightMap& rhs);


public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   WeightMap               (const DagRaster& dr,
                                            const geo::ScalarSimpleRaster& dem);

  /* virtual */    ~WeightMap              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  //! get rid off, only needed for CellLoc<-->LinearLoc
  geo::RasterDim rasterDim() const;

  double get(const Edge& e) const;

  double operator[](const Edge& e) const
    { return get(e); }

  void   fillDirMap(geo::NB::Code dir,
                    REAL4         *map) const;

  static double mvMark()
   { return std::numeric_limits<double>::max(); }
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



} // namespace mldd

#endif
