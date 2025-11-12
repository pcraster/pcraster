#ifndef INCLUDED_CALC_DECOMPRESSEDDATA
#define INCLUDED_CALC_DECOMPRESSEDDATA

#include "stddefx.h"
#include "vsenum.h"


namespace calc {
  // DecompressedData declarations.
}

namespace calc {


/*! Keep single instance of data.
 *  If the decompressed copy is kept, then that copy is deleted in dtor.
 */
struct DecompressedData
{

private:
  VS                   d_vs;

  // Assignment operator.
  DecompressedData&           operator=           (const DecompressedData&);

  // Copy constructor.
                 DecompressedData               (const DecompressedData&);

public:
  const void*          d_originalData{nullptr};
  const unsigned char* d_decompressedCopy{nullptr};


  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   DecompressedData               (VS vs);

  /* virtual */    ~DecompressedData              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
  void setOriginalData(const void *originalData);
  void setDecompressedCopy(const unsigned char *decompressedCopy);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  const void *decompressed() const;
  VS          vs()   const { return d_vs; }

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
