#ifndef INCLUDED_CALC_DECOMPRESSEDDATA
#define INCLUDED_CALC_DECOMPRESSEDDATA



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
  const void*          d_originalData;
  const unsigned char* d_decompressedCopy;


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
