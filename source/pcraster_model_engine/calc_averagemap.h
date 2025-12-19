#ifndef INCLUDED_CALC_AVERAGEMAP
#define INCLUDED_CALC_AVERAGEMAP

#include "stddefx.h"
#include "csftypes.h"
#include "com_statistics.h"

#include <map>


namespace calc {
  // AverageMap declarations.
}



namespace calc {

struct AreaAverageMap : std::map<INT4,com::AverageMinMax< REAL4 > >
{
  void operator()(INT4 id, REAL4 v) {
    operator[](id)(v);
  }
  void setResults(double *res, INT4 nrVals) const;
};


//! as used for timeseries
class AverageMap
{

private:

  //! Assignment operator. NOT IMPLEMENTED.
  AverageMap&           operator=           (AverageMap const& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   AverageMap               (AverageMap const& rhs);

  typedef        com::AverageMinMax< REAL4 > A;
  AreaAverageMap         d_map;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   AverageMap               ();

  /* virtual */    ~AverageMap              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
  template<typename IDF>
   void apply(const IDF   *id,  size_t idLen,
          const REAL4 *val, size_t valLen);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  void                   setResults          (double *res, size_t nrVals) const;
  const AreaAverageMap& areaAverageMap       () const;

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
