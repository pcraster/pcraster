#ifndef INCLUDED_OLDCALC_COMPRESSIONINPUT
#define INCLUDED_OLDCALC_COMPRESSIONINPUT

#include "stddefx.h"
#include "vsenum.h"
#include "calc_valuebuffer.h"


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
  ValueBuffer       d_decompressedData{};

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
  VS      vs() const { return d_vs; }

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
