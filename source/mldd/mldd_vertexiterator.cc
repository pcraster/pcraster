#include "stddefx.h"
#include "mldd_vertexiterator.h"



/*!
  \file
  This file contains the implementation of the VertexIterator class.
*/



//------------------------------------------------------------------------------

/*
namespace mldd {

class VertexIteratorPrivate
{
public:

  VertexIteratorPrivate()
  {
  }

  ~VertexIteratorPrivate()
  {
  }

};

} // namespace mldd
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC VERTEXITERATOR MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF VERTEXITERATOR MEMBERS
//------------------------------------------------------------------------------


//! not default constructible
mldd::VertexIterator::VertexIterator():
  d_g(nullptr),
  d_v()
{
}


mldd::VertexIterator::VertexIterator(const DagRaster& g,
                                     Vertex v):
  d_g(&g),
  d_v(v)
{
}


//! Copy constructor.
mldd::VertexIterator::VertexIterator(VertexIterator const& rhs)
  : d_g(rhs.d_g), d_v(rhs.d_v)
{
}



mldd::VertexIterator::~VertexIterator()
{
}



//! Assignment operator.
mldd::VertexIterator& mldd::VertexIterator::operator=(VertexIterator const& rhs)
{
  if (this != &rhs) {
    d_v = rhs.d_v;
    d_g = rhs.d_g;
  }
  return *this;
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



