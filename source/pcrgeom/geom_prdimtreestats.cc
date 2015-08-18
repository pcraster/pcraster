#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_GEOM_PRDIMTREESTATS
#include "geom_prdimtreestats.h"
#define INCLUDED_GEOM_PRDIMTREESTATS
#endif

// Library headers.

// PCRaster library headers.

// Module headers.



/*!
  \file
  This file contains the implementation of the PRDimTreeStats class.
*/



//------------------------------------------------------------------------------

/*
namespace geom {

class PRDimTreeStatsPrivate
{
public:

  PRDimTreeStatsPrivate()
  {
  }

  ~PRDimTreeStatsPrivate()
  {
  }

};

} // namespace geom
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC PRDIMTREESTATS MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF PRDIMTREESTATS MEMBERS
//------------------------------------------------------------------------------


geom::PRDimTreeStats::PRDimTreeStats():
  nrInternalNodes(0),nrLeafNodes(0),nrElements(0),memSize(0)
{
}

geom::PRDimTreeStats&
 geom::PRDimTreeStats::operator+=(const PRDimTreeStats& rhs)
{
  nrInternalNodes+=rhs.nrInternalNodes;
  nrLeafNodes    +=rhs.nrLeafNodes;
  nrElements     +=rhs.nrElements;
  memSize        +=rhs.memSize;
  return *this;
}


//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------

std::ostream &operator<<(std::ostream &s, const geom::PRDimTreeStats &stats)
{
 s << "nrInternalNodes" << stats.nrInternalNodes << "\n";
 s << "nrLeafNodes    " << stats.nrLeafNodes     << "\n";
 s << "nrElements     " << stats.nrElements      << "\n";
 s << "memSize        " << stats.memSize         << "\n";
 return s;
}


//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



