#ifndef INCLUDED_CALC_MASKPACKING
#define INCLUDED_CALC_MASKPACKING



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
#ifndef INCLUDED_CALC_SPATIALPACKING
#include "calc_spatialpacking.h"
#define INCLUDED_CALC_SPATIALPACKING
#endif
#ifndef INCLUDED_CALC_VS
#include "calc_vs.h"
#define INCLUDED_CALC_VS
#endif


namespace calc {
  // MaskPacking declarations.
}



namespace calc {


/*! Only the datavalues the resides within the mask (e.g. the areamap) in
 *  a Spatial object are stored  within the
 *  runtime environment (RunTimeEnv). Thus a Spatial can have less values than
 *  the number of  values computed by the rasterDim
 *  This packing method eliminates computation on the missing values.
 */
class MaskPacking: public SpatialPacking
{
public:
   typedef std::vector<bool>        Mask;
private:

   template<typename T>
     inline void decompress(T* dest, const T *src) const;
   template<typename T>
     inline void compress(T* dest, const T *src) const;

   friend class MaskPackingTest;

  //! Assignment operator. NOT IMPLEMENTED.
  MaskPacking&           operator=           (const MaskPacking&);

  //  Copy constructor default for createClone
  //               MaskPacking               (const MaskPacking&);


   //! translate index, size equals nr of 1's in mask ctor arg
   std::vector<size_t>        d_compressedToDecompressed;

   //! translate index, size equals nr of cells in the rasterDim
   std::vector<size_t>        d_decompressedToCompressed;

   /*!
    * alternating indexes starting a value sequence or a MV
    * sequence. By definition is d_rlIndex 0 and d_rlIndex.back()
    * rs.nrCells() that equals past-the-end in the decompressed
    * maps
    */
   std::vector<size_t>        d_rlIndex;
   //! 1 if the even elements of d_rlIndex are value sequences, 0 if not
   size_t                     d_evenIsValueRL;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   MaskPacking(const geo::RasterDim& rs,
                               const Mask&           mask);

  /* virtual */    ~MaskPacking              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  Field*         pack                (const Field* f)const;
  const Field*   unpack              (const Field* f)const;
  Field*         createSpatial       (VS vs)const;

  size_t         toRasterId          (size_t compressedIndex) const;
  size_t         toFieldId           (size_t compressedIndex) const;
  size_t         nrFieldCells        () const;

  MaskPacking*   createClone         () const;
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
