#include "ag_VectorDataSources.h"

// External headers.

// Project headers.

// Module headers.



/*!
  \file
  This file contains the implementation of the VectorDataSources class.
*/

namespace {

} // Anonymous namespace



namespace ag {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC VECTORDATASOURCES MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF VECTORDATASOURCES MEMBERS
//------------------------------------------------------------------------------

VectorDataSources::VectorDataSources()

  : DataObjectBase<Vector>(geo::VECTOR)

{
}



VectorDataSources::~VectorDataSources()
{
}



DataInfo<Vector> VectorDataSources::openData(
         std::string const& name,
         dal::DataSpace const& space) const
{
  std::auto_ptr<Vector> vector(new Vector(name, space));
  assert(vector.get());

  DataInfo<Vector> info(vector.get(), VS_SCALAR, vector->dataSpace());
  vector.release();

  return info;
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

} // namespace ag

