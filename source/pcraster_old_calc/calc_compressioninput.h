#ifndef INCLUDED_CALC_COMPRESSIONINPUT
#define INCLUDED_CALC_COMPRESSIONINPUT



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_VSENUM
#include "vsenum.h"
#define INCLUDED_VSENUM
#endif
#ifndef INCLUDED_CALC_VALUEBUFFER
#include "calc_valuebuffer.h"
#define INCLUDED_CALC_VALUEBUFFER
#endif

namespace calc {
  // CompressionInput declarations.
}



namespace calc {

class Compressor;

//! Manage a decompressed buffers for input data ready to compress
class CompressionInput
{

private:

  //! Assignment operator. NOT IMPLEMENTED.
  CompressionInput&           operator=           (const CompressionInput&);

  //! Copy constructor. NOT IMPLEMENTED.
                   CompressionInput               (const CompressionInput&);

  VS                d_vs;
  ValueBuffer       d_decompressedData;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   CompressionInput               (VS vs,
                                                   void *decompressedData,
                                                   const Compressor& compressor);

  /* virtual */    ~CompressionInput              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
  void   *detachData();

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  void *decompressedData() const;
  VS      vs() const { return d_vs; };

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
