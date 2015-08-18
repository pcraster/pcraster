#ifndef INCLUDED_DAL_FEATUREDRIVER
#include "dal_FeatureDriver.h"
#define INCLUDED_DAL_FEATUREDRIVER
#endif

// Library headers.

// PCRaster library headers.

// Module headers.



/*!
  \file
  This file contains the implementation of the FeatureDriver class.
*/



namespace dal {

// Code that is private to this module.
namespace detail {

} // namespace detail



//------------------------------------------------------------------------------
// DEFINITION OF STATIC FEATUREDRIVER MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FEATUREDRIVER MEMBERS
//------------------------------------------------------------------------------

FeatureDriver::FeatureDriver(
         Format const& format)

  : Driver(format)

{
}



FeatureDriver::~FeatureDriver()
{
}



/*!
  \overload
  \warning   .
  \sa        .
*/
DataSpace FeatureDriver::dataSpace(
         std::string const& name,
         DataSpace const& space,
         DataSpaceAddress const& address) const
{
  assert(!space.hasSpace());

  boost::shared_ptr<FeatureLayer> layer(open(name, space, address));

  if(!layer) {
    throwCannotBeOpened(name, FEATURE, space, address);
  }

  DataSpace result;

  SpaceDimensions spaceDimensions(layer->dimensions().west(),
         layer->dimensions().north(), layer->dimensions().east(),
         layer->dimensions().south());

  result.addDimension(Dimension(Space, BorderedDiscretisation,
         spaceDimensions));

  return result;
}



FeatureLayer* FeatureDriver::open(
         std::string const& name,
         TypeId typeId) const
{
  return open(name, DataSpace(), DataSpaceAddress(), typeId);
}



FeatureLayer* FeatureDriver::open(
         std::string const& name,
         DataSpace const& space,
         DataSpaceAddress const& address) const
{
  assert(!space.hasSpace());

  return open(name, space, address, TI_NR_TYPES);
}



FeatureLayer* FeatureDriver::read(
         std::string const& name) const
{
  return read(name, DataSpace(), DataSpaceAddress(), TI_NR_TYPES);
}



FeatureLayer* FeatureDriver::read(
         std::string const& name,
         DataSpace const& space,
         DataSpaceAddress const& address) const
{
  return read(name, space, address, TI_NR_TYPES);
}



void FeatureDriver::read(
         FeatureLayer& layer,
         std::string const& name) const
{
  read(layer, name, DataSpace(), DataSpaceAddress());
}



bool FeatureDriver::extremes(
         boost::any& min,
         boost::any& max,
         TypeId typeId,
         std::string const& name,
         DataSpace const& space) const
{
  assert(!space.hasSpace());

  bool result = false;

  switch(typeId) {
    case TI_UINT1: {
      UINT1 i, a;
      if(extremes<UINT1>(i, a, name, space, typeId)) {
        min = i;
        max = a;
        result = true;
      }

      break;
    }
    case TI_INT4: {
      INT4 i, a;
      if(extremes<INT4>(i, a, name, space, typeId)) {
        min = i;
        max = a;
        result = true;
      }

      break;
    }
    case TI_REAL4: {
      REAL4 i, a;
      if(extremes<REAL4>(i, a, name, space, typeId)) {
        min = i;
        max = a;
        result = true;
      }

      break;
    }
    default: {
      assert(false);
      break;
    }
  }

  return result;
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

} // namespace dal

