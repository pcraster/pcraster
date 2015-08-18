#ifndef INCLUDED_COM_CPUCYCLECOUNTER
#define INCLUDED_COM_CPUCYCLECOUNTER



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



namespace com {
  // CpuCycleCounter declarations.
}



namespace com {



//! short_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
/*!
  longer_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
*/
class CpuCycleCounter
{
public:
  typedef unsigned long long Uint64;
private:
  friend class CpuCycleCounterTest;

  struct Interval {
    //! accumulates intervals between start and next stop
    Uint64 d_total;
    //! records start time, at start
    Uint64 d_start;
    Interval();
  };

  std::vector<Interval>   d_counters;

  //! Assignment operator. NOT IMPLEMENTED.
  CpuCycleCounter&           operator=           (CpuCycleCounter const& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   CpuCycleCounter               (CpuCycleCounter const& rhs);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   CpuCycleCounter               (size_t nrCounters=1);

  /* virtual */    ~CpuCycleCounter              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
  void             start                         (size_t counter);
  void             stop                          (size_t counter);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  std::vector<size_t>  counters                   (size_t maxCount=10000) const;

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



} // namespace com

#endif
