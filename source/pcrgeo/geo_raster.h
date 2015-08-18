#ifndef INCLUDED_GEO_RASTER
#define INCLUDED_GEO_RASTER

#ifndef INCLUDED_GEO_RASTERSPACE
#include "geo_rasterspace.h"
#define INCLUDED_GEO_RASTERSPACE
#endif

#ifndef INCLUDED_GEO_SIMPLERASTER
#include "geo_simpleraster.h"
#define INCLUDED_GEO_SIMPLERASTER
#endif



namespace geo {



/*!
  \class Raster
  \brief A raster map with associated data values

  Implements a rectangular raster with square cells of data.
  For each cell space for a data value is reserved.
*/
template<class T>
class Raster: public SimpleRaster<T>
{

private:

  //! Properties of the raster.
  RasterSpace      d_space;

  //! Assignment operator. NOT IMPLEMENTED.
  Raster &         operator=           (const Raster &);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   Raster              (size_t nr,
                                        size_t nc,
                                        double cellSize,
                                        double left,
                                        double top,
                                        Projection proj = YIncrB2T);

                   Raster              (const RasterSpace& rs);

  //! Copy constructor.
                   Raster              (const Raster &rhs);

  //! Destructor.
  virtual          ~Raster             ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  const RasterSpace& space             () const;

  double           left                () const;

  double           right               () const;

  double           back                () const;

  double           front               () const;

  double           top                 () const;

  double           bottom              () const;

  double           width               () const;

  double           height              () const;

  double           cellSize            () const;

  double           angle               () const;

  Projection       projection          () const;

};



//------------------------------------------------------------------------------
// INLINE FUNCIONS
//------------------------------------------------------------------------------

template<class T>
inline double Raster<T>::left() const
{
  return d_space.left();
}

template<class T>
inline double Raster<T>::right() const
{
  return d_space.right();
}

template<class T>
inline double Raster<T>::back() const
{
  return d_space.back();
}

template<class T>
inline double Raster<T>::front() const
{
  return d_space.front();
}

template<class T>
inline double Raster<T>::top() const
{
  return d_space.top();
}

template<class T>
inline double Raster<T>::bottom() const
{
  return d_space.bottom();
}

template<class T>
inline double Raster<T>::width() const
{
  return d_space.width();
}

template<class T>
inline double Raster<T>::height() const
{
  return d_space.height();
}

template<class T>
inline double Raster<T>::cellSize() const
{
  return d_space.cellSize();
}

template<class T>
inline double Raster<T>::angle() const
{
  return d_space.angle();
}

template<class T>
inline Projection Raster<T>::projection() const
{
  return d_space.projection();
}



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------

typedef Raster<UINT1>        UINT1Raster;
typedef Raster<INT4>         INT4Raster;
typedef Raster<REAL4>        REAL4Raster;

// typedef Raster<BoolType>     BoolRaster;
// typedef Raster<NomType>      NomRaster;
// typedef Raster<OrdType>      OrdRaster;
// typedef Raster<ScalType>     ScalRaster;
// typedef Raster<DirectType>   DirectRaster;
// typedef Raster<LddType>      LddRaster;



//------------------------------------------------------------------------------



} // namespace geo



#endif
