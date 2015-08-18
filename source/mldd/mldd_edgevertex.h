#ifndef INCLUDED_MLDD_EDGEVERTEX
#define INCLUDED_MLDD_EDGEVERTEX

#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.
#ifndef INCLUDED_GEO_NEIGHBOUR
#include "geo_neighbour.h"
#define INCLUDED_GEO_NEIGHBOUR
#endif

// Module headers.



namespace geo {
   class RasterDim;
}



namespace mldd {

  typedef geo::CellLoc Vertex;

  class  Edge  {
    Vertex d_source,d_target;
   public:
    Edge() {};
    Edge(const Vertex& source, const Vertex& target):
      d_source(source),d_target(target) {};

    const Vertex& source() const  {
      return d_source;
    }
    const Vertex& target() const {
      return d_target;
    }
    bool operator== (const Edge& e) const {
      return e.target() == target() &&
             e.source() == source();
    }
    bool operator!= (const Edge& e) const {
      return e.target() != target() ||
             e.source() != source();
    }

    void linear(size_t& source,
                size_t& target,
                const geo::RasterDim& rd) const;
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



} // namespace mldd

#endif
