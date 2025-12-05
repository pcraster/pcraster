#ifndef INCLUDED_MLDD_REMOVESTREAM
#define INCLUDED_MLDD_REMOVESTREAM

#include "stddefx.h"
#include "pcrtypes.h"
#include "mldd_downstreamvisitor.h"
#include "mldd_dagraster.h"

#include <vector>



namespace mldd {

//! removeStream implementation
/*!
 * implementation is choosen such for performance, such that the 8 sized d_marks
 * vector is only accessed within the d_dag graph.
 */
class RemoveStream : public DownstreamVisitor
{

  friend class RemoveStreamTest;

  const DagRaster&                  d_dag;
  DagRaster                         d_removeDag;
  std::vector<const UINT1 *>        d_marks;


private:

  //! Assignment operator. NOT IMPLEMENTED.
  RemoveStream&           operator=           (RemoveStream const& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   RemoveStream               (RemoveStream const& rhs);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   RemoveStream               (
                    const DagRaster&                 dag,
                    const std::vector<const UINT1 *>& marksInArgOrder);


  /* virtual */   ~RemoveStream              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
  void             downstreamEdge            (const Edge& e);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  const DagRaster& removeDag                 ()const;

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
