#ifndef INCLUDED_CALC_GRIDMAP
#define INCLUDED_CALC_GRIDMAP



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.
#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif

// PCRaster library headers.
#ifndef INCLUDED_GEO_RASTERSPACE
#include "geo_rasterspace.h"
#define INCLUDED_GEO_RASTERSPACE
#endif

// Module headers.
#ifndef INCLUDED_CALC_TYPES
#include "calc_types.h"
#define INCLUDED_CALC_TYPES
#endif
#ifndef INCLUDED_CALC_GRIDSTAT
#include "calc_gridstat.h"
#define INCLUDED_CALC_GRIDSTAT
#endif


namespace dal {
  // GridMap declarations.
  class RasterDriver;
}



namespace calc {
class SpatialPacking;
class Field;

//! a map with a raster structure
class GridMap
{
protected:
  const std::string d_fileName;
  geo::RasterSpace  d_rs;
  VS                d_vs;

//GridMap(const std::string& fileName,size_t nrRows,size_t nrCols, VS vs);

  GridMap(const std::string& fileName);

private:

  //               GridMap               ();

  //! Assignment operator. NOT IMPLEMENTED.
  GridMap&           operator=           (const GridMap&);

  //! Copy constructor. NOT IMPLEMENTED.
                   GridMap               (const GridMap&);

public:
  virtual        ~GridMap              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  const std::string& fileName() const { return d_fileName; };
  VS                       vs() const { return d_vs; };
  size_t            nrCells() const { return nrRows()*nrCols(); };
  size_t             nrCols() const { return d_rs.nrCols(); };
  size_t             nrRows() const { return d_rs.nrRows(); };
  geo::RasterSpace const& rasterSpace() const { return d_rs;};

};

class GridMapOut : public GridMap
{
private:

                   GridMapOut               ();

  //! Assignment operator. NOT IMPLEMENTED.
  GridMapOut&           operator=           (const GridMapOut&);

  //! Copy constructor. NOT IMPLEMENTED.
                   GridMapOut               (const GridMapOut&);

  //! write each grid cell
  /*!
   * \param allValues array with for each grid cell a value
   */
  GridStat writeData(const void *allValues);

  dal::RasterDriver const&  d_driver;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------
  GridMapOut(const std::string&   fileName,
             dal::RasterDriver const&  driver,
             const geo::RasterSpace& rs, VS vs);


  virtual        ~GridMapOut              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  GridStat writeNonSpatial(const void *value);

  GridStat writeSpatial(const void *values);


  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
};

class GridMapIn : public GridMap {
  bool               d_bandMap;
public:
  GridMapIn(std::string const& fileName);
  ~GridMapIn();
  void    createSpatial(void *dest,VS vs);
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
