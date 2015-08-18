#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_AVERAGEMAP
#include "calc_averagemap.h"
#define INCLUDED_CALC_AVERAGEMAP
#endif

// Library headers.
#ifndef INCLUDED_COM_MVGENERIC
#include "com_mvgeneric.h"
#define INCLUDED_COM_MVGENERIC
#endif

// PCRaster library headers.

// Module headers.



/*!
  \file
  This file contains the implementation of the AverageMap class.
*/



//------------------------------------------------------------------------------

/*
namespace calc {

class AverageMapPrivate
{
public:

  AverageMapPrivate()
  {
  }

  ~AverageMapPrivate()
  {
  }

};

} // namespace calc
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC AVERAGEMAP MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF AVERAGEMAP MEMBERS
//------------------------------------------------------------------------------

calc::AverageMap::AverageMap()
{
}



/* NOT IMPLEMENTED
//! Copy constructor.
calc::AverageMap::AverageMap(AverageMap const& rhs)

  : Base(rhs)

{
}
*/



calc::AverageMap::~AverageMap()
{
}



/* NOT IMPLEMENTED
//! Assignment operator.
calc::AverageMap& calc::AverageMap::operator=(AverageMap const& rhs)
{
  if (this != &rhs) {
  }
  return *this;
}
*/

//! on base of ID Field type (UINT1/INT4)
template<typename IDF>
void calc::AverageMap::apply(
    const IDF   *id,  size_t idLen,
    const REAL4 *val, size_t valLen)
{
  // FTTB only one call allowed
  PRECOND(d_map.empty());
  if (idLen == 1) {
    // optimization, else clause can do this also
    //  but this eliminates the lookups in d_map
    d_map.insert(std::make_pair(id[0],
                 com::forEachNonMV(val,val+valLen,A())));
  } else
    d_map =
      com::iterateNonMV2(id,idLen, val,valLen,d_map);
}

template void calc::AverageMap::apply<UINT1>(
    const UINT1   *id,  size_t idLen,
    const REAL4 *val, size_t valLen);
template void  calc::AverageMap::apply<INT4>(
    const INT4    *id,  size_t idLen,
    const REAL4 *val, size_t valLen);

void calc::AverageMap::setResults(double *res, size_t nrVals) const
{
  pcr::setMV(res,nrVals);
  d_map.setResults(res,nrVals);
}

void calc::AreaAverageMap::setResults(double *res, INT4 nrVals) const
{
 for(const_iterator i=begin(); i!=end();++i) {
   if(i->first > 0 && i->first <= nrVals)
      res[i->first-1]=i->second.average(); // id 1 at col 0
 }
}

const calc::AreaAverageMap&
 calc::AverageMap::areaAverageMap() const
{
  return d_map;
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



