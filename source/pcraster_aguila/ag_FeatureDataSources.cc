#include "ag_FeatureDataSources.h"

// External headers.

// Project headers.

// Module headers.



/*!
  \file
  This file contains the implementation of the FeatureDataSources class.
*/



namespace ag {

// Code that is private to this module.
namespace detail {

} // namespace detail



//------------------------------------------------------------------------------
// DEFINITION OF STATIC FEATUREDATASOURCES MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FEATUREDATASOURCES MEMBERS
//------------------------------------------------------------------------------

FeatureDataSources::FeatureDataSources()

  : DataObjectBase<FeatureLayer>(geo::FEATURE)

{
}



FeatureDataSources::~FeatureDataSources()
{
}



DataInfo<FeatureLayer> FeatureDataSources::openData(
         std::string const& name,
         dal::DataSpace const& space) const
{
  std::auto_ptr<FeatureLayer> layer(new FeatureLayer(name, space));
  assert(layer.get());

  DataInfo<FeatureLayer> info(layer.get(), layer->valueScale(),
         layer->dataSpace());
  layer.release();

  return info;
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

} // namespace ag

