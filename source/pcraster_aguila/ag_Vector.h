#ifndef INCLUDED_AG_VECTOR
#define INCLUDED_AG_VECTOR



// External headers.
#include <boost/scoped_ptr.hpp>

// Project headers.
#include "dal_Vector.h"

// Module headers.
#include "ag_RasterDataset.h"



namespace ag {
  // Vector declarations.
}



namespace ag {

//! short_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
/*!
  longer_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED

  \sa        .
*/
class Vector: public RasterDataset
{

  friend class VectorTest;

private:

  boost::scoped_ptr<dal::Vector> _vector;

  dal::DataSpace   _space;

  bool             isRead              (dal::DataSpaceAddress const& address) const;

protected:

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   Vector              (std::string const& name,
                                        dal::DataSpace const& space);

  /* virtual */    ~Vector             ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  void             read                (dal::DataSpace const& space,
                                        dal::DataSpaceAddress const& address);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  dal::RasterDimensions const& dimensions() const;

  bool             isRead              () const;

  size_t           nrRows              () const;

  size_t           nrCols              () const;

  double           cellSize            () const;

  dal::TypeId      typeId              () const;

  template<typename T>
  bool             isMV                (size_t row,
                                        size_t col) const;

  bool             isMV                (size_t row,
                                        size_t col) const;

  template<typename T>
  T const&         x                   (size_t row,
                                        size_t col) const;

  template<typename T>
  T const&         y                   (size_t row,
                                        size_t col) const;

  template<typename T>
  void             value               (T& result,
                                        size_t row,
                                        size_t col) const;

  template<typename T>
  bool             value               (T& result,
                                        dal::DataSpace const& space,
                                        dal::DataSpaceAddress const& address) const;

  template<typename T>
  void             angle               (T& result,
                                        size_t row,
                                        size_t col) const;

};



//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------

template<typename T>
inline T const& Vector::x(
         size_t row,
         size_t col) const
{
  return _vector->x<T>(row, col);
}

template<typename T>
inline T const& Vector::y(
         size_t row,
         size_t col) const
{
  return _vector->y<T>(row, col);
}

template<typename T>
inline void Vector::value(
         T& result,
         size_t row,
         size_t col) const
{
  _vector->cell<T>(result, row, col);
}

template<typename T>
inline bool Vector::value(
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
          value<T>(result, static_cast<size_t>(row), static_cast<size_t>(col));
        }
      }
    }
  }

  return !pcr::isMV(result);
}



template<typename T>
void Vector::angle(
         T& result,
         size_t row,
         size_t col) const
{
  T const& x = this->x<T>(row, col);
  T const& y = this->y<T>(row, col);

  if(pcr::isMV(x) || pcr::isMV(y)) {
    pcr::setMV(result);
  }
  else {
    result = dal::clockwiseAngle(x, y);
  }
}



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------

} // namespace ag

#endif
