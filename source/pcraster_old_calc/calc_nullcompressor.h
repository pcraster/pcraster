#ifndef INCLUDED_OLDCALC_NULLCOMPRESSOR
#define INCLUDED_OLDCALC_NULLCOMPRESSOR

#include "stddefx.h"
#include "calc_compressor.h"



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

  /* virtual */    ~NullCompressor              () override;

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  void  decompress    (DecompressedData& d,
                       const void *data)  const override;
  Spatial          *createSpatial (CompressionInput& ci)const override;
  size_t toDecompressedIndex(size_t linIndexCompressed) const override;
  size_t            nrCellsCompressed() const override;

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
