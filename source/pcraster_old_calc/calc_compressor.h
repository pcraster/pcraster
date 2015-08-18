#ifndef INCLUDED_CALC_COMPRESSOR
#define INCLUDED_CALC_COMPRESSOR



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.
#ifndef INCLUDED_VSENUM
#include "vsenum.h"
#define INCLUDED_VSENUM
#endif
#ifndef INCLUDED_GEO_RASTERSPACE
#include "geo_rasterspace.h"
#define INCLUDED_GEO_RASTERSPACE
#endif

// Module headers.



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
