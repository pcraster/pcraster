#ifndef INCLUDED_GEO_NEIGHBOUR
#define INCLUDED_GEO_NEIGHBOUR



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.
#ifndef INCLUDED_STACK
#include <stack>
#define INCLUDED_STACK
#endif

// PCRaster library headers.
#ifndef INCLUDED_GEO_CELLLOC
#include "geo_cellloc.h"
#define INCLUDED_GEO_CELLLOC
#endif

// Module headers.


namespace geo {



//------------------------------------------------------------------------------
// INLINE FUNCIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------

//! encode the 8 neighbours
/* A one byte Neighbour bit code can be used to encode multiple neighbours.
 * Bit nrs set refer to the following target vertex, relative to X the source
 * vertex.
 * <pre>
 *  5   6   7
 *   \  |  /
 *  3 - X - 4
 *    / |  \
 *  0   1   2
 * </pre>
 */

struct NB {
  //! value between 0 and 8
  typedef unsigned int Code;
  static bool valid(Code c) {
    return c < 8;
  }
  static  Code reverse(Code c) {
    DEVELOP_PRECOND(valid(c));
    return 7-c;
  }
  // one of the 8 neigbours
  static CellLoc   target(const CellLoc& from, Code dir);
  static Code      code  (const CellLoc& from, const CellLoc& to);
  static bool      diagonal(const CellLoc& from, const CellLoc& to);
  static LinearLoc target(LinearLoc      from,
                          Code dir,
                          size_t     nrCells,
                          size_t     nrColls);
  static /* LDD::Code */unsigned int toLDD(Code c) {
    DEVELOP_PRECOND(valid(c));
    return c+1+(c>5);
  }
};

//! encode a local drain direction, the 8 neighbours
/*! local drain direction maps have values for directions as
 *  following:
 *  7   8   9
 *   \  |  /
 *  4 - 5 - 6
 *    / |  \
 *  1   2   3
 */
struct LDD {
  //! value between 1 and 9
  typedef unsigned int Code;
  static bool valid(Code c) {
    return c>=1 && c <= 9;
  }

  //! the reverse code
  /*!
   * if a cell has and outflow code of nbCode
   * then the recieving cell has an inflow code
   * of nrRevCode(nbCode) and vice versa.
   */
  static  Code reverse(Code c) {
    // DEVELOP_PRECOND(valid(c));
    // current version of calc::UpstreamNeighbourVisitor
    // does also check invalid codes to test
    // replace that class with something like the 
    // mldd::OutEdgeIterator
    return 10-c;
  }

  static CellLoc   target(const CellLoc& from, Code dir);
  static LinearLoc target(LinearLoc      from,
                          Code dir,
                          size_t     nrCells,
                          size_t     nrCols);

  // one of the 8 neigbours or cell itself (pit)
  static CellLoc downstreamCell(const CellLoc& from, Code dir) {
    return target(from,dir);
  }

  //! does these two ldd values point to each other?
  static bool flowsTo(Code c1, Code c2)
  {
     return c1==reverse(c2);
  }
  static NB::Code toNB(Code c) {
    DEVELOP_PRECOND(valid(c));
    DEVELOP_PRECOND(c!=5); // cannnot convert pit
    return c-1-(c>5);
  }
};

//! ldd (outflow) value to outflow bit code
// size_t geo::lddVal2OutflowNBCode(size_t lddVal)  --> LDD::toNB
// NB::Code  lddVal2InflowNBCode(LDD::Code lddVal); --> NB::reverse(LDD::toNB)

void intDownstreamCell(int& rowNB,int& colNB,
                            const geo::CellLoc& from, LDD::Code fromLddVal);

//! stores a coordinate (row,col) and a visitor of its upstream neighbour
/*!
   see also  geo::DownStreamVisitorCell for details of bit array
   and looping
 */
class UpstreamNeighbourVisitor : public CellLoc {
  private:
     //! bit array recording the upstream neighbours
     unsigned short int d_upstreamNeighbourDirs; // only 9 bits used, all dirs
     //! the next neighbour to inspect if it is an upstream neighbour
     unsigned short int d_nextNeighbourToVisit; // only 4 bits used 2^4>9
  public:
     UpstreamNeighbourVisitor(
      const CellLoc& l, unsigned short int upstreamNeighbourDirs);

     void operator++();

     //! is there still something to visit
     bool valid() const
     {
      return d_nextNeighbourToVisit == 10;
     }
     CellLoc nb() const;
};

//! keeps track of where we are
/*!
    we use the trick that pointing to a neighbour
    of a current cell
    by using ldd code i, the neighbour must have
    value 10-i if it points to the current cell:
     it is an upstream neighbour
*/
class DownStreamVisitorCell : public CellLoc {
 public:
   //! bit array recording the upstream neighbours
   /*! bit # corresponds with ldd direction value
    */
   unsigned short int d_upstreamNeighbourDirs:9; // only 9 bits used, all dirs
   //! the next neighbour to inspect if it is an upstream neighbour
   unsigned short int d_nextNeighbourToVisit:4; // only 4 bits used 2^4>9

   //! ctor
   DownStreamVisitorCell(const CellLoc& l):
     CellLoc(l),
     d_upstreamNeighbourDirs(0),
     d_nextNeighbourToVisit(0)
  {
  }
  //! dtor
  ~DownStreamVisitorCell() {};

  //! are all upstream neighbours visited?
  bool allUpstreamNeighboursVisited() const {
   return d_nextNeighbourToVisit==10;
  }
  //! set next to visit and mark current if needed
  void next(bool markCurrentAsUpstreamNB) {
    if (markCurrentAsUpstreamNB)
      d_upstreamNeighbourDirs |= 1 << d_nextNeighbourToVisit;
    // skip pit value 5
    d_nextNeighbourToVisit += (d_nextNeighbourToVisit==4)? 2 : 1;
  }
};

} // namespace geo

#endif
