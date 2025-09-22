#include "stddefx.h"
#include "calc_scopedlddgraph.h"
#include "calc_runtimeenv.h"
#include "calc_field.h"
#include "calc_lddgraph.h"



/*!
  \file
  This file contains the implementation of the ScopedLddGraph class.
*/



//------------------------------------------------------------------------------

/*
namespace calc {

class ScopedLddGraphPrivate
{
public:

  ScopedLddGraphPrivate()
  {
  }

  ~ScopedLddGraphPrivate()
  {
  }

};

} // namespace calc
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC SCOPEDLDDGRAPH MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF SCOPEDLDDGRAPH MEMBERS
//------------------------------------------------------------------------------

calc::ScopedLddGraph::ScopedLddGraph(RunTimeEnv* rte, const Field& ldd):
 d_lg(rte,&ldd)
{
 POSTCOND(ldd.vs()==VS_L);
 if (!d_lg.object())
  d_lg.setObject(new LddGraph(ldd.src_1(),rte->ifieldRDConversion()));
}


/* NOT IMPLEMENTED
//! Copy constructor.
calc::ScopedLddGraph::ScopedLddGraph(ScopedLddGraph const& rhs)

  : Base(rhs)

{
}
*/



calc::ScopedLddGraph::~ScopedLddGraph()
{
  delete d_modifiedLg;
}



/* NOT IMPLEMENTED
//! Assignment operator.
calc::ScopedLddGraph& calc::ScopedLddGraph::operator=(ScopedLddGraph const& rhs)
{
  if (this != &rhs) {
  }
  return *this;
}
*/

/*!
 * \brief create a modified graph as current() if mvAtInput 
 *        has MV's at graph vertices
 * \param mvAtInput on input the MV's in both graph and non-vertices (MV's
 *        in Field that created the graph) on output only MV's in the
 *        current graph before the possible change of current in this call.
 */
void calc::ScopedLddGraph::setMVInput(BitField& mvAtInput)
{
  d_lg.object()->unsetMVField(mvAtInput);
  if (mvAtInput.any()) {
    auto *n = new LddGraph(current(),mvAtInput,true);
    delete d_modifiedLg;
    d_modifiedLg=n;
  }
}

//! return the current graph
const calc::LddGraph& calc::ScopedLddGraph::current() const
{
  if (d_modifiedLg)
    return *d_modifiedLg;
  return *(d_lg.object());
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



