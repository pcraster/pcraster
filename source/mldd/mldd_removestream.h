#ifndef INCLUDED_MLDD_REMOVESTREAM
#define INCLUDED_MLDD_REMOVESTREAM



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.

// Module headers.

// Library headers.
#ifndef INCLUDED_VECTOR
#include <vector>
#define INCLUDED_VECTOR
#endif

// PCRaster library headers.
#ifndef INCLUDED_PCRTYPES
#include "pcrtypes.h"
#define INCLUDED_PCRTYPES
#endif
// Module headers.
#ifndef INCLUDED_MLDD_DOWNSTREAMVISITOR
#include "mldd_downstreamvisitor.h"
#define INCLUDED_MLDD_DOWNSTREAMVISITOR
#endif
#ifndef INCLUDED_MLDD_DAGRASTER
#include "mldd_dagraster.h"
#define INCLUDED_MLDD_DAGRASTER
#endif



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
