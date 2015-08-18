#ifndef INCLUDED_CALC_GRIDMAP
#define INCLUDED_CALC_GRIDMAP



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.
// CW TODO ergens een partiele fwd definitie?
#include <string>

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_VSENUM
#include "vsenum.h"
#define INCLUDED_VSENUM
#endif



namespace calc {
  // GridMap declarations.
}



namespace calc {
class Compressor;
class Spatial;

//! a map with a raster structure
class GridMap
{
protected:
  std::string const d_fileName;
  size_t d_nrRows,d_nrCols;
  VS     d_vs;

  GridMap(const std::string& fileName,size_t nrRows,size_t nrCols, VS vs);

  GridMap(const std::string& fileName);

private:

                   GridMap               ();

  //! Assignment operator. NOT IMPLEMENTED.
  GridMap&           operator=           (const GridMap&);

  //! Copy constructor. NOT IMPLEMENTED.
                   GridMap               (const GridMap&);

  virtual void readInBuffer(VS readAs, void *val)=0;
public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------


  virtual        ~GridMap              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
  //! write each grid cell
  /*!
   * \param allValues array with for each grid cell a value
   */
  virtual void writeData(const void *allValues)=0;

  void writeNonSpatial(const void *value);

  void writeSpatial(const void *values);

  //! read map data, return allocated buffer with value stream
  /*! if val is not 0, that buffer is used and no buffer is allocated
   */
  Spatial* readData(VS readAs, const Compressor& c);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  const std::string& fileName() const { return d_fileName; };
  VS                       vs() const { return d_vs; };
  size_t            nrCells() const { return nrRows()*nrCols(); };
  size_t             nrCols() const { return d_nrCols; };
  size_t             nrRows() const { return d_nrRows; };

  //! returns true if min and max are set, false if they are untouched
  virtual bool getMinMax(double& min, double& max)const =0;

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
