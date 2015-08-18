#ifndef INCLUDED_DAL_VECTOR
#define INCLUDED_DAL_VECTOR



// External headers.
#ifndef INCLUDED_BOOST_SHARED_PTR
#include <boost/shared_ptr.hpp>
#define INCLUDED_BOOST_SHARED_PTR
#endif

// Project headers.
#ifndef INCLUDED_CMATH
#include <cmath>
#define INCLUDED_CMATH
#endif

// Module headers.
#ifndef INCLUDED_DAL_CONFIGURE
#include "dal_Configure.h"
#define INCLUDED_DAL_CONFIGURE
#endif

#ifndef INCLUDED_DAL_DATASET
#include "dal_Dataset.h"
#define INCLUDED_DAL_DATASET
#endif

#ifndef INCLUDED_DAL_MATRIX
#include "dal_Matrix.h"
#define INCLUDED_DAL_MATRIX
#endif

#ifndef INCLUDED_DAL_RASTERDIMENSIONS
#include "dal_RasterDimensions.h"
#define INCLUDED_DAL_RASTERDIMENSIONS
#endif



namespace dal {
  // Vector declarations.
  class Matrix;
}



namespace dal {

//! A class for raster based vector data.
/*!
  A raster based vector attribute is defined by a magnitude in x-direction and
  a magnitude in y-direction.

  \sa        .
*/
class PCR_DAL_DECL Vector: public Dataset
{

  friend class VectorTest;

private:

  //! Properties of the raster.
  RasterDimensions _dimensions;

  //! Type id of the values.
  TypeId           _typeId;

  //! Magnitude in x-direction.
  boost::shared_ptr<Matrix> _x;

  //! Magnitude in y-direction.
  boost::shared_ptr<Matrix> _y;

  //! Minimum values, when calculated and available.
  boost::any       _min;

  //! Maximum values, when calculated and available.
  boost::any       _max;

  template<typename T>
  void             calculateExtremes   ();

protected:

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   Vector              (RasterDimensions const& dimensions,
                                        TypeId typeId);

                   Vector              (Vector const& rhs);

  /* virtual */    ~Vector             ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  Vector&          operator=           (Vector const& rhs);

  void             setTypeId           (TypeId typeId);

  void             createCells         ();

  void             transfer            (Matrix& x,
                                        Matrix& y);

  void             setAllMV            ();

  void             calculateExtremes   ();

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  RasterDimensions const& dimensions   () const;

  size_t           nrRows              () const;

  size_t           nrCols              () const;

  size_t           nrCells             () const;

  double           cellSize            () const;

  double           north               () const;

  double           south               () const;

  double           west                () const;

  double           east                () const;

  TypeId           typeId              () const;

  bool             cellsAreCreated     () const;

  bool             hasExtremes         () const;

  // bool             allMV               () const;

  void const*      xCells              () const;

  void*            xCells              ();

  template<typename T>
  T const*         xCells              () const;

  template<typename T>
  T*               xCells              ();

  void const*      yCells              () const;

  void*            yCells              ();

  template<typename T>
  T const*         yCells              () const;

  template<typename T>
  T*               yCells              ();

  template<typename T>
  T const&         x                   (size_t row,
                                        size_t col) const;

  template<typename T>
  T const&         y                   (size_t row,
                                        size_t col) const;

  template<typename T>
  void             cell                (T& value,
                                        size_t row,
                                        size_t col) const;

  template<typename T>
  T                min                 () const;

  template<typename T>
  T                max                 () const;

};



//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------

template<typename T>
inline T const* Vector::xCells() const
{
  return _x->cells<T>();
}

template<typename T>
inline T* Vector::xCells()
{
  return _x->cells<T>();
}

template<typename T>
inline T const* Vector::yCells() const
{
  return _y->cells<T>();
}

template<typename T>
inline T* Vector::yCells()
{
  return _y->cells<T>();
}

template<typename T>
inline T const& Vector::x(
         size_t row,
         size_t col) const
{
  return _x->cell<T>(row, col);
}

template<typename T>
inline T const& Vector::y(
         size_t row,
         size_t col) const
{
  return _y->cell<T>(row, col);
}

template<typename T>
inline T Vector::min() const
{
  return boost::any_cast<T>(_min);
}

template<typename T>
inline T Vector::max() const
{
  return boost::any_cast<T>(_max);
}

template<typename T>
inline void Vector::cell(
         T& value,
         size_t row,
         size_t col) const
{
  T const& x = this->x<T>(row, col);
  T const& y = this->y<T>(row, col);

  if(!pcr::isMV(x) && !pcr::isMV(y)) {
    // TODO this template is instantiated for non-float types, hence the cast.
    // TODO update calling code to not instantiate for non-float types.
    value = static_cast<T>(std::sqrt(double(x * x + y * y)));
  }
  else {
    pcr::setMV(value);
  }
}

//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------

} // namespace dal

#endif
