#include "ag_RasterDataset.h"

// External headers.

// Project headers.

// Module headers.



/*!
  \file
  This file contains the implementation of the RasterDataset class.
*/

namespace {

} // Anonymous namespace



namespace ag {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC RASTERDATASET MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF RASTERDATASET MEMBERS
//------------------------------------------------------------------------------

RasterDataset::RasterDataset(
         std::string const& name,
         dal::DataSpace const& space)

  : SpatialDataset(name, space)

{
}



RasterDataset::~RasterDataset()
{
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

} // namespace ag

