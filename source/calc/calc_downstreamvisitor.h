#ifndef INCLUDED_CALC_DOWNSTREAMVISITOR
#define INCLUDED_CALC_DOWNSTREAMVISITOR



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
#ifndef INCLUDED_FIELDAPI_INTERFACE
#include "fieldapi_interface.h" // ReadOnlyUint1
#define INCLUDED_FIELDAPI_INTERFACE
#endif
#ifndef INCLUDED_GEO_CELLLOC
#include "geo_cellloc.h"
#define INCLUDED_GEO_CELLLOC
#endif
#ifndef INCLUDED_GEO_NEIGHBOUR
#include "geo_neighbour.h"
#define INCLUDED_GEO_NEIGHBOUR
#endif

// Module headers.


namespace calc {

//! implements a downstream visiting scheme for a single catchment
/*!
   DownStreamVisitor assures the order of cells returned is such
   that all cells upstream of a cell returned are already returned
   earlier.
   <br>
   Modelled after \class geo::CellLocVisitor
*/
class DownStreamVisitor
{
private:
   //! list of cells to processed
   /*!
       This is a path from d_current to its pit, with pit as last element
    */
   std::stack<geo::DownStreamVisitorCell>    d_inProcess;
   //! ldd map
   const fieldapi::ReadOnlyUint1&             d_lddMap;

  //! Assignment operator. NOT IMPLEMENTED.
  DownStreamVisitor&           operator=    (const DownStreamVisitor&);

  //! Copy constructor. NOT IMPLEMENTED.
                   DownStreamVisitor        (const DownStreamVisitor&);

  geo::DownStreamVisitorCell& front();

  void next();

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   DownStreamVisitor       (
                                     const fieldapi::ReadOnlyUint1& lddMap,
                                     const geo::CellLoc& catchmentOutlet);

  /* virtual */   ~DownStreamVisitor       ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  void operator++();

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  bool valid() const;

  geo::UpstreamNeighbourVisitor operator*() const;

};



//------------------------------------------------------------------------------
// INLINE FUNCIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------

} // namespace calc

#endif
