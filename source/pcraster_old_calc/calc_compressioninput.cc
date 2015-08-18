#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_COMPRESSIONINPUT
#include "calc_compressioninput.h"
#define INCLUDED_CALC_COMPRESSIONINPUT
#endif

// Library headers.

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_CALC_COMPRESSOR
#include "calc_compressor.h"
#define INCLUDED_CALC_COMPRESSOR
#endif
#ifndef INCLUDED_CALC_MAP2CSF
#include "calc_map2csf.h"
#define INCLUDED_CALC_MAP2CSF
#endif


/*!
  \file
  This file contains the implementation of the CompressionInput class.
*/


//------------------------------------------------------------------------------
// DEFINITION OF STATIC COMPRESSIONINPUT MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF COMPRESSIONINPUT MEMBERS
//------------------------------------------------------------------------------

/*!
 * \param vs vs
 * \param decompressedData can be 0, if not this will own and delete
 */
calc::CompressionInput::CompressionInput(VS vs,
                                         void *decompressedDataOwnAndDelete,
                                         const Compressor& compressor):
 d_vs(vs)
{
 d_decompressedData.d_void=decompressedDataOwnAndDelete;

  size_t len = compressor.rasterSpace().nrCells();
  if (!d_decompressedData.d_void)
    d_decompressedData = createValueBuffer(biggestCellRepr(vs), len);
}

/*!
 * return decompressed data and let callee own it, this will not delete the data
 * anymore
 */
void *calc::CompressionInput::detachData()
{
  return detach(d_decompressedData);
}

calc::CompressionInput::~CompressionInput()
{
  deallocate(d_decompressedData);
}

void *calc::CompressionInput::decompressedData() const
{
  return d_decompressedData.d_void;
}


//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



