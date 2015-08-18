#ifndef INCLUDED_CALC_MASKCOMPRESSOR
#define INCLUDED_CALC_MASKCOMPRESSOR



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.
#ifndef INCLUDED_VECTOR
#include <vector>
#define INCLUDED_VECTOR
#endif

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_CALC_COMPRESSOR
#include "calc_compressor.h"
#define INCLUDED_CALC_COMPRESSOR
#endif
#ifndef INCLUDED_CALC_VS
#include "calc_vs.h"
#define INCLUDED_CALC_VS
#endif


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

  /* virtual */    ~MaskCompressor              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  void              decompress(DecompressedData& d,
                               const void * compressedData) const;
  void             *compress  (VS vs,
                               void *compressedValueBuffer,
                               void *decompressedValueBuffer) const;
  Spatial          *createSpatial(CompressionInput& ci)const;

  size_t            toDecompressedIndex(size_t compressedIndex) const;
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
