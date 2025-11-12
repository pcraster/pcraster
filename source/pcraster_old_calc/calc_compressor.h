#ifndef INCLUDED_CALC_COMPRESSOR
#define INCLUDED_CALC_COMPRESSOR

#include "stddefx.h"
#include "vsenum.h"
#include "geo_rasterspace.h"



namespace calc {
  // Compressor declarations.
}



namespace calc {

struct DecompressedData;
class CompressionInput;
class Spatial;

class Compressor
{

private:

  //! Assignment operator. NOT IMPLEMENTED.
  Compressor&           operator=           (const Compressor&);

  //! Copy constructor. NOT IMPLEMENTED.
                   Compressor               (const Compressor&);

  //! the raster space it acts on
  const geo::RasterSpace  d_rs;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   Compressor               (const geo::RasterSpace& rs);

     virtual       ~Compressor              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  const geo::RasterSpace&  rasterSpace() const;
  virtual void              decompress(DecompressedData& d,
                                      const void * compressedData) const=0;

  virtual Spatial *createSpatial(CompressionInput& ci)const=0;
  virtual size_t   toDecompressedIndex(size_t linIndexCompressed) const=0;
  virtual size_t   nrCellsCompressed() const=0;

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
