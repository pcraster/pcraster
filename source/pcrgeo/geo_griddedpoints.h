#ifndef INCLUDED_GEO_GRIDDEDPOINTS
#define INCLUDED_GEO_GRIDDEDPOINTS



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.
#ifndef INCLUDED_LIST
#include <list>
#define INCLUDED_LIST
#endif

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_GEO_RASTERSPACE
#include "geo_rasterspace.h"
#define INCLUDED_GEO_RASTERSPACE
#endif

#ifndef INCLUDED_GEO_SIMPLERASTER
#include "geo_simpleraster.h"
#define INCLUDED_GEO_SIMPLERASTER
#endif



namespace geo {
  // GriddedPoints declarations.
  class Neighbourhood;
}



namespace geo {



//! short_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
/*!
  longer_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
*/
template<class Point>
class GriddedPoints
{

public:

  typedef std::list<Point> List;

private:

  // Keep a list of points to prevent invalidation of pointers to the list
  // items.
  typedef geo::SimpleRaster<List> PointsRaster;
  typedef geo::SimpleRaster<bool> MVRaster;

  RasterSpace      d_space;

  PointsRaster     d_points;

  MVRaster         d_missingValues;


  // Assignment operator. Default.
  // GriddedPoints&   operator=           (const GriddedPoints& rhs);

public:

  typedef typename List::const_iterator const_iterator;
  typedef typename List::iterator iterator;

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   GriddedPoints       (const RasterSpace& space);

                   GriddedPoints       (const RasterSpace& space,
                                        const MVRaster& missingValues);

                   GriddedPoints       (const GriddedPoints& rhs);

  /* virtual */    ~GriddedPoints      ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  void             setMV               (size_t row,
                                        size_t col);

  void             setMV               (const CellLoc& loc);

  iterator         insert              (const Point& point);

  void             clear               ();

  void             remove              (iterator it,
                                        size_t row,
                                        size_t col);

  void             remove              (const Point& point,
                                        size_t row,
                                        size_t col);

  void             move                (iterator it,
                                        size_t row,
                                        size_t col);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  const RasterSpace& space             () const;

  const MVRaster&  missingValues       () const;

  List const&      cell                (LinearLoc loc) const;

  List&            cell                (LinearLoc loc);

  const List&      cell                (const CellLoc& loc) const;

  List&            cell                (const CellLoc& loc);

  const_iterator   begin               (size_t row,
                                        size_t col) const;

  const_iterator   end                 (size_t row,
                                        size_t col) const;

  iterator         begin               (size_t row,
                                        size_t col);

  iterator         end                 (size_t row,
                                        size_t col);

  iterator         begin               (const CellLoc& loc);

  iterator         end                 (const CellLoc& loc);

  const_iterator   begin               (const CellLoc& loc) const;

  const_iterator   end                 (const CellLoc& loc) const;

  bool             empty               () const;

  bool             empty               (LinearLoc loc) const;

  bool             empty               (size_t row,
                                        size_t col) const;

  size_t           size                () const;

  size_t           size                (size_t row,
                                        size_t col) const;

  size_t           size                (const CellLoc& loc) const;

  bool             isMV                (LinearLoc loc) const;

  bool             isMV                (size_t row,
                                        size_t col) const;

  bool             isMV                (const CellLoc& loc) const;

  template<class Location>
  CellLoc          cellLoc             (const Location& point) const;

  template<class Location>
  LinearLoc        linearLoc           (const Location& point) const;

  size_t           nrRows              () const;

  size_t           nrCols              () const;

  size_t           nrCells             () const;

  size_t           nrPoints            () const;

  size_t           nrPoints            (size_t row,
                                        size_t col) const;

  size_t           nrPoints            (const CellLoc& loc) const;

  template<class Location>
  void             points              (const Location& point,
                                        double radius,
                                        std::vector<Point>& points) const;

  template<class Location>
  void             points              (const Location& point,
                                        const Neighbourhood& neigbourhood,
                                        std::vector<Point>& points) const;

  void             points              (const CellLoc& loc,
                                        double radius,
                                        std::vector<Point>& points) const;

  void             points              (const CellLoc& loc,
                                        const Neighbourhood& neigbourhood,
                                        std::vector<Point>& points) const;

  void             points              (const CellLoc& loc,
                                        const Neighbourhood& neigbourhood,
                                        std::vector<Point*>& points);

  void             points              (const CellLoc& loc,
                                        double radius,
                                        std::vector<Point*>& points);

  template<class Location>
  void             points              (const Location& point,
                                        double radius,
                                        std::vector<Point*>& points);

};



//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------

template<class Point>
template<class Location>
inline geo::CellLoc geo::GriddedPoints<Point>::cellLoc(
         Location const& point) const
{
  double r, c;

  d_space.coords2RowCol(point[0], point[1], r, c);

  PRECOND(r >= 0.0);
  PRECOND(c >= 0.0);

  size_t row = static_cast<size_t>(r);
  size_t col = static_cast<size_t>(c);

  PRECOND(row < nrRows());
  PRECOND(col < nrCols());

  return CellLoc(row, col);
}

template<class Point>
template<class Location>
inline geo::LinearLoc geo::GriddedPoints<Point>::linearLoc(
         Location const& point) const
{
  CellLoc loc = cellLoc(point);

  return loc.row() + nrCols() + loc.col();
}

template<class Point>
template<class Location>
inline void geo::GriddedPoints<Point>::points(const Location& point,
         double radius, std::vector<Point>& points) const
{
  this->points(cellLoc<Location>(point), radius, points);
}

template<class Point>
template<class Location>
inline void geo::GriddedPoints<Point>::points(const Location& point,
         double radius, std::vector<Point*>& points)
{
  this->points(cellLoc<Location>(point), radius, points);
}

template<class Point>
template<class Location>
inline void geo::GriddedPoints<Point>::points(const Location& point,
         const Neighbourhood& neighbourhood, std::vector<Point>& points) const
{
  this->points(cellLoc<Location>(point), neighbourhood, points);
}



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace geo

#endif
