#include "stddefx.h"
#include "mldd_removestream.h"
#include "mldd_dagraster.h"
#include "mldd_arg2nborder.h"



/*!
  \file
  This file contains the implementation of the RemoveStream class.
*/



//------------------------------------------------------------------------------

/*
namespace mldd {

class RemoveStreamPrivate
{
public:

  RemoveStreamPrivate()
  {
  }

  ~RemoveStreamPrivate()
  {
  }

};

} // namespace mldd
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC REMOVESTREAM MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF REMOVESTREAM MEMBERS
//------------------------------------------------------------------------------

mldd::RemoveStream::RemoveStream(
    const DagRaster&    dag,
    const std::vector<const UINT1 *>& marksInArgOrder):
     DownstreamVisitor(dag.rasterDim()),
     d_dag(dag),
     d_removeDag(dag.rasterDim()),
     d_marks(8)
{
  arg2NBOrder(d_marks,marksInArgOrder);
}

/* NOT IMPLEMENTED
//! Copy constructor.
mldd::RemoveStream::RemoveStream(RemoveStream const& rhs)

  : Base(rhs)

{
}
*/



mldd::RemoveStream::~RemoveStream()
{
}



/* NOT IMPLEMENTED
//! Assignment operator.
mldd::RemoveStream& mldd::RemoveStream::operator=(RemoveStream const& rhs)
{
  if (this != &rhs) {
  }
  return *this;
}
*/

/*!
 * note that initVertex is already called on both e.source() and e.target()
 *  DownstreamVisitor garantuees that
 */
void mldd::RemoveStream::downstreamEdge (const Edge& e)
{
  size_t s = 0;
  size_t t = 0;
  size_t nb= geo::NB::code(e.source(),e.target());
  e.linear(s,t,d_dag.rasterDim());
  // delete e if marked in d_marks
  // or all infows of e.source are marked for delettion in d_removeDag
  if (d_marks[nb][s]==1 ||
      (d_removeDag.isVertex(s)
      && d_dag.nrInflowNB(e.source()) == d_removeDag.nrInflowNB(e.source()))
      ) {
    d_removeDag.addFlow(e.source(),e.target());
  }
}

const mldd::DagRaster& mldd::RemoveStream::removeDag() const
{
  return d_removeDag;
}


//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



