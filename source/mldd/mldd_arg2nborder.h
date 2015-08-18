#ifndef INCLUDED_MLDD_ARG2NBORDER
#define INCLUDED_MLDD_ARG2NBORDER



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
#ifndef INCLUDED_GEO_NEIGHBOUR
#include "geo_neighbour.h"
#define INCLUDED_GEO_NEIGHBOUR
#endif

// Module headers.

namespace mldd {
  template<class T> void arg2NBOrder(
      std::vector<T>&  nb,
      const std::vector<T>&  arg)
  {
    PRECOND(nb.size()==8);
    PRECOND(arg.size()==8);
    // put in geo_neighbour.h as northClockWiseOrder
    // argument order is        N,NE,E,SE,S,SW,W,NW
    //                          3 4  5 6  7 8  9 10
    // geo::LDD::Code lddCodes[8]={8,9 ,6,3 ,2,1 ,4,7 };
    geo::NB::Code   nbCodes[8]={6,7 ,4,2 ,1,0 ,3,5 };
    for(size_t i=0; i < 8; ++i) {
      PRECOND(!nb[nbCodes[i]]); // assuming 0 init
      PRECOND(arg[i]);          // assming a ptr value
      nb[nbCodes[i]] = arg[i];
      PRECOND(nb[nbCodes[i]]); // assuming ptr init
    }
  }
}

#endif
