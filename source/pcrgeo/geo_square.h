#ifndef INCLUDED_GEO_SQUARE
#define INCLUDED_GEO_SQUARE

#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_GEO_POINT
#include "geo_point.h"
#define INCLUDED_GEO_POINT
#endif

namespace geo {

 //! on boundary is outside
 struct OpenBoundaries {
  template <typename T>
   static bool outside(T point, T centre, T halfWidth) {
     return std::fabs(point-centre) >= halfWidth;
   }
  static const char *name() { return "Open"; }
 };
 //! on boundary is inside
 struct ClosedBoundaries {
  template <typename T>
   static bool outside(T point, T centre, T halfWidth) {
     return std::fabs(point-centre) > halfWidth;
   }
  static const char *name() { return "Closed"; }
 };
 //! left/lower is closed, right/higher is open
 struct ClosedOpenBoundaries {
  template <typename T>
   static bool outside(T point, T centre, T halfWidth) {
     T diff=centre-point;
     if (diff < 0)
      return diff <= -halfWidth;
     return diff > halfWidth;
   }
   static const char *name() { return "ClosedOpen"; }
 };

 //! left/lower is open, right/higher is closed
 struct OpenClosedBoundaries {
  template <typename T>
   static bool outside(T point, T centre, T halfWidth) {
     T diff=centre-point;
     if (diff < 0)
      return diff <  -halfWidth;
     return diff >= halfWidth;
   }
   static const char *name() { return "OpenClosed"; }
 };


//! a "square" in \a n dimensional space with \a T as coordinate type.
/*! In 2D it is named q square in 3D a cube, equal size in all dimensions.
 */
template<
  typename T,
  size_t   n,
  typename Boundaries = ClosedBoundaries >
class Square
{

private:
  typedef          Point<T,n> P;
  P                d_centre;
  T                d_halfWidth;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

  //! Constructor.
                   Square              ():
                     d_centre(),d_halfWidth() {};

                   Square              (const Point<T,n> centre,
                                        T                halfWidth):
                     d_centre(centre),d_halfWidth(halfWidth) {};

                   Square            (const Square &rhs):
                     d_centre(rhs.d_centre),
                     d_halfWidth(rhs.d_halfWidth)
                   {}

  //! Destructor.
  /* virtual */    ~Square              () {};

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //! Assignment operator.
  Square &          operator=           (const Square &rhs) {
   if(this != &rhs)
   {
     d_centre=rhs.centre();
     d_halfWidth=rhs.halfWidth();
   }
   return *this;
  }

  void setHalfWidth(T halfWidth) {
    d_halfWidth=halfWidth;
  }

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  const Point<T,n>& centre() const {
    return d_centre;
  }
  T halfWidth() const {
    return d_halfWidth;
  }

  Square quadSquareAt    (size_t indexDirection ) const;
  bool   contains        (const Point<T,n>&   p)  const;
  bool   intersects      (const Square&       s)  const;

};



//------------------------------------------------------------------------------
// INLINE FUNCIONS
//------------------------------------------------------------------------------

/*!
 * In 2D terms: return definition of one of the quadrants. Which quadrant
 * is defined by the indexDirection.
 * indexDirection is explained in Point::indexDirection
 */
template<typename T, size_t n, typename B >
 Square<T,n,B >
  Square<T, n,B >::quadSquareAt(size_t indexDirection) const
{
  P c(centre());
  T h=d_halfWidth/2;
  for(size_t i = 0; i < n; ++i)
    if (indexDirection & (1<<i))
      c[i]-=h;
    else
      c[i]+=h;
  return Square<T,n,B>(c,h);
}

template<class T, size_t n, typename B>
 bool
  Square<T, n, B>::contains(const Point<T,n>&   p)  const
{
  for(size_t i = 0; i < n; ++i) {
   if (B::outside(p[i],d_centre[i],d_halfWidth))
     return false;
  }
  return true;
}

//! check if intersects with other square
/*!
 * \todo
 *  specialization for 1 and 2D that compute min distance requried
 *  in each dimension for no intersection
 */
template<class T, size_t n, typename B>
  bool Square<T, n, B>::intersects  (const Square&     other)  const
{
  /* catch the case where one is contained
   * in the other, since below we only
   * catch partial overlap
   */
   if (contains(other.centre()) ||
       other.contains(centre()) )
     return true;
  /* if one of the edges is within the other
   * it intersects for boths partly
   */
  T side[2]={1,-1};
  for(size_t s=0; s<(1<<n); ++s) {
    // bitfield s encode for each dimension
    // a permutation of 1/-1 to select edge
    // coordinates
    P p(centre());
    P po(other.centre());
    for(size_t i = 0; i < n; ++i) {
      // !-negation just to cast to 0,1 range
      // !=0 would also do
      p[i]+=side[!(s&(1<<i))]*      halfWidth();
     po[i]+=side[!(s&(1<<i))]*other.halfWidth();
	}
    if (other.contains(p)||contains(po))
        return true;
  }
  return false;
}




//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace geo

template<class T, size_t n, typename B>
std::ostream &operator<<(std::ostream &s, const geo::Square<T, n,B> &sq)
{
  s << "c:" << sq.centre() << " hw:" << sq.halfWidth() << " " << B::name();
  return s;
}

#endif
