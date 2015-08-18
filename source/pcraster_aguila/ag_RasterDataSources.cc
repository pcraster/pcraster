#include "ag_RasterDataSources.h"

// Library headers.

// PCRaster library headers.
#include "dal_DataSpaceAddress.h"
#include "dal_StackInfo.h"
#include "dal_Utils.h"

// Module headers.



/*!
  \file
  This file contains the implementation of the RasterDataSources class.
*/



//------------------------------------------------------------------------------

/*
namespace ag {

class RasterDataSourcesPrivate
{
public:

  RasterDataSourcesPrivate()
  {
  }

  ~RasterDataSourcesPrivate()
  {
  }

};

} // namespace ag
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC RASTERDATASOURCES MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF RASTERDATASOURCES MEMBERS
//------------------------------------------------------------------------------

ag::RasterDataSources::RasterDataSources()

  : DataObjectBase<Raster>(geo::STACK)

{
}



ag::RasterDataSources::~RasterDataSources()
{
}



ag::DataInfo<ag::Raster> ag::RasterDataSources::openData(
         std::string const& name,
         dal::DataSpace const& space) const
{
  std::auto_ptr<Raster> raster(new Raster(name, space));
  assert(raster.get());

  /*
  std::set<size_t> timeSteps;
  if(space.hasTime()) {
    size_t index = space.indexOf(dal::Time);
    dal::Dimension dimension = space.dimension(index);
    assert(dimension.meaning() == dal::Time);
    assert(dimension.type() == dal::Dimension::NUMERICAL);
    assert(dimension.discretisation() == dal::RegularDiscretisation);
    assert(dimension.nrValues() == 3);
    size_t first = dimension.value<size_t>(0);
    size_t last = dimension.value<size_t>(1);
    size_t increment = dimension.value<size_t>(2);
    for(size_t i = first; i <= last; i += increment) {
      timeSteps.insert(i);
    }
  }
  */

  DataInfo<Raster> info(raster.get(), raster->valueScale(),
         raster->dataSpace());
  raster.release();

  return info;
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



