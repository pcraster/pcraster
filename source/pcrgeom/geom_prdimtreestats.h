#ifndef INCLUDED_GEOM_PRDIMTREESTATS
#define INCLUDED_GEOM_PRDIMTREESTATS



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

#ifndef INCLUDED_IOSTREAM
#include <iostream>
#define INCLUDED_IOSTREAM
#endif
// PCRaster library headers.

// Module headers.



namespace geom {
  // PRDimTreeStats declarations.
}



namespace geom {


struct PRDimTreeStats {
    size_t nrInternalNodes;
    //! non-empty leaf nodes
    size_t nrLeafNodes;
    size_t nrElements;
    size_t memSize;
   PRDimTreeStats();
   PRDimTreeStats& operator+=(const PRDimTreeStats& rhs);
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



} // namespace geom

std::ostream &operator<<(std::ostream &s, const geom::PRDimTreeStats &stats);

#endif
