#ifndef INCLUDED_AG_RASTER
#define INCLUDED_AG_RASTER



// Library headers.
#include <cassert>
#include <boost/lexical_cast.hpp>

// PCRaster library headers.
#include "dal_DataSpace.h"
#include "dal_DataSpaceAddress.h"
#include "dal_Raster.h"
#include "dal_RasterDimensions.h"
#include "dal_SpatialCoordinate.h"

// Module headers.
#include "ag_RasterDataset.h"



namespace dal {
}
namespace ag {
  // Raster declarations.
}



namespace ag {



//! Wrapper class for rasters, optimized for use in Aguila.
/*!
  Yet another raster class, this time for use in visualisation library.

  This class knows about rasters and how to read them.
*/
class Raster: public RasterDataset
{

  friend class RasterTest;

private:

  dal::Raster*     d_raster;

  dal::RasterDimensions d_dimensions;

  dal::DataSpace   d_dataSpace;

  CSF_VS           d_valueScale;

  // bool             hasExtremes         () const;

  bool             isRead              (dal::DataSpaceAddress const& address) const;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   Raster              (std::string const& name,
                                        dal::DataSpace const& space);

  /* virtual */    ~Raster             ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  void             read                (dal::DataSpace const& space,
                                        dal::DataSpaceAddress const& address);

  // template<typename T>
  // bool             cell                (T& value,
  //                                       dal::DataSpace const& space,
  //                                       dal::DataSpaceAddress const& address);


  // void             setDataSpaceAddressMapper(dal::DataSpaceAddressMapper* mapper);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  dal::RasterDimensions const& dimensions() const;

  CSF_VS           valueScale          () const;

  dal::TypeId      typeId              () const;

  bool             isRead              () const;

  bool             isMV                (size_t row,
                                        size_t col) const;

  // bool             allMV               () const;

  /*
  template<typename T>
  bool             isMV                () const;
  */

  template<typename T>
  T const*         cells               () const;

  template<typename T>
  T const&         cell                (size_t row,
                                        size_t col) const;

  template<typename T>
  std::set<T>      classes             () const;

  /// dal::DataSource const& source        () const;

  template<typename T>
  bool             value               (T& result,
                                        dal::DataSpace const& space,
                                        dal::DataSpaceAddress const& address) const;

  bool             hasLegend           () const;

  dal::Table       legend              () const;

};



//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------

/*
template<typename T>
inline bool Raster::isMV() const
{
  assert(d_raster);
  return d_raster->cells<T>().elements();
}
*/

template<typename T>
inline T const* Raster::cells() const
{
  assert(d_raster);
  return d_raster->cells<T>();
}

template<typename T>
inline T const& Raster::cell(size_t row, size_t col) const
{
  assert(d_raster);
  return d_raster->cell<T>(d_dimensions.index(row, col));
}

//!
/*!
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .
  \todo      Implement
*/
template<typename T>
inline std::set<T> Raster::classes() const
{
  std::set<T> values;
  dataSource().template uniqueValues<T>(values, d_raster->typeId());
  return values;
}

template<typename T>
inline bool Raster::value(
         T& result,
         dal::DataSpace const& space,
         dal::DataSpaceAddress const& address) const
{
  pcr::setMV(result);

  if(Dataset::isRead(space, address)) {
    size_t index = space.indexOf(dal::Space);

    // Check whether the data space has space dimensions.
    // Rasters might be empty (contain no cells at all).
    if(index != space.size()) {
      if(address.isValid(index)) {
        dal::RasterDimensions const& rasterDimensions(
              space.dimension(index).value<dal::RasterDimensions>(0));
        dal::SpatialCoordinate const& spatialAddress(
              address.template coordinate<dal::SpatialCoordinate>(index));
        double row, col;

        rasterDimensions.indices(spatialAddress, row, col);

        if(rasterDimensions.containsCell(row, col)) {
          if(!pcr::isMV(cell<T>(static_cast<size_t>(row),
              static_cast<size_t>(col)))) {
            result = cell<T>(static_cast<size_t>(row),
              static_cast<size_t>(col));
          }
        }
      }
    }
  }

  return !pcr::isMV(result);
}

template<>
inline bool Raster::value<std::string>(
         std::string& result,
         dal::DataSpace const& space,
         dal::DataSpaceAddress const& address) const
{
  assert(d_raster);

  result = "";

  switch(d_raster->typeId()) {
    case dal::TI_UINT1: {
      UINT1 value;

      if(this->value<UINT1>(value, space, address)) {
        result = boost::lexical_cast<std::string>(INT4(value));
      }

      break;
    }
    case dal::TI_INT4: {
      INT4 value;

      if(this->value<INT4>(value, space, address)) {
        result = boost::lexical_cast<std::string>(value);
      }

      break;
    }
    case dal::TI_REAL4: {
      REAL4 value;

      if(this->value<REAL4>(value, space, address)) {
        result = boost::lexical_cast<std::string>(value);
      }

      break;
    }
    default: {
      assert(false);
      break;
    }
  }

  return !result.empty();
}



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace ag

#endif
