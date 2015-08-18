#ifndef INCLUDED_BLOCK_DEHAANCOMPACTOR
#define INCLUDED_BLOCK_DEHAANCOMPACTOR



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.
#ifndef INCLUDED_PCRTYPES
#include "pcrtypes.h"
#define INCLUDED_PCRTYPES
#endif

// Module headers.



namespace block {
  // DeHaanCompactor declarations.
}



namespace block {



//! Compaction functor based on the method by De Haan.
/*!
  Given three parameters, the initial thickness, cumulative load and duration
  this functor can calculate the compacted thickness of a layer of sediment.

  \todo Add ref to book De Haan.
*/
class DeHaanCompactor
{

  friend class DeHaanCompactorTest;

private:

  //! Parameter of compaction function.
  double           d_b;

  //! Parameter of compaction function.
  double           d_c;

  //! Parameter of compaction function.
  double           d_buoyancy;

protected:

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   DeHaanCompactor     ();

                   DeHaanCompactor     (double b,
                                        double c,
                                        double buoyancy);

  /* virtual */    ~DeHaanCompactor    ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  REAL4            operator()          (REAL4 initialThickness,
                                        REAL4 cummulativeLoad,
                                        double duration) const;

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  double           buoyancy            () const;

  bool             equals              (DeHaanCompactor const& compactor) const;

};



//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------

bool               operator==          (DeHaanCompactor const& lhs,
                                        DeHaanCompactor const& rhs);

bool               operator!=          (DeHaanCompactor const& lhs,
                                        DeHaanCompactor const& rhs);



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace block

#endif
