#ifndef INCLUDED_CALC_NULLCOMPRESSOR
#define INCLUDED_CALC_NULLCOMPRESSOR



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.
#ifndef INCLUDED_CALC_COMPRESSOR
#include "calc_compressor.h"
#define INCLUDED_CALC_COMPRESSOR
#endif

// Module headers.



namespace calc {
  // NullCompressor declarations.
}



namespace calc {



//! Compressor that does not compress but stubs that actions
class NullCompressor: public Compressor
{

private:

  //! Assignment operator. NOT IMPLEMENTED.
  NullCompressor&           operator=           (const NullCompressor&);

  //! Copy constructor. NOT IMPLEMENTED.
                   NullCompressor               (const NullCompressor&);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   NullCompressor               (const geo::RasterSpace& rs);

  /* virtual */    ~NullCompressor              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  void  decompress    (DecompressedData& d,
                       const void *data)  const;
  Spatial          *createSpatial (CompressionInput& ci)const;
  size_t toDecompressedIndex(size_t linIndexCompressed) const;
  size_t            nrCellsCompressed() const;

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
