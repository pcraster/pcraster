#ifndef INCLUDED_OLDCALC_MASKCOMPRESSOR
#define INCLUDED_OLDCALC_MASKCOMPRESSOR

#include "stddefx.h"
#include "calc_compressor.h"
#include "calc_vs.h"

#include <vector>



namespace calc {
  // MaskCompressor declarations.
}



namespace calc {



//! a compressor that eliminates computation on the missing values
class MaskCompressor: public Compressor
{

private:

  //! Assignment operator. NOT IMPLEMENTED.
  MaskCompressor&           operator=           (const MaskCompressor&);

  //! Copy constructor. NOT IMPLEMENTED.
                   MaskCompressor               (const MaskCompressor&);


   //! 0 means MV, 1 means value in compressed buffer
   std::vector<bool>        d_mask;

   //! translate index, size equals nr of 1's in d_mask
   std::vector<size_t>        d_compressedToDecompressed;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   MaskCompressor(const geo::RasterSpace& rs,
                                  const unsigned char * const mask);

  /* virtual */    ~MaskCompressor              () override;

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  void              decompress(DecompressedData& d,
                               const void * compressedData) const override;
  void             *compress  (VS vs,
                               void *compressedValueBuffer,
                               void *decompressedValueBuffer) const;
  Spatial          *createSpatial(CompressionInput& ci)const override;

  size_t            toDecompressedIndex(size_t compressedIndex) const override;
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
