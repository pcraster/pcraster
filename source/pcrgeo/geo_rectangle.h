#ifndef INCLUDED_GEO_RECTANGLE
#define INCLUDED_GEO_RECTANGLE



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CSFTYPES
#include "csftypes.h"
#define INCLUDED_CSFTYPES
#endif

#ifndef INCLUDED_GEO_POINT
#include "geo_point.h"
#define INCLUDED_GEO_POINT
#endif



/*!
  \class geo_Rectangle
  \brief The geo_Rectangle class is for rectangle objects.

  geo_Rectangle is a class for rectangle objects.
  CW: Are these object in a projection or simply cartesian?
  \sa geo::Square
  \sa http://qsoft.ragestorm.net/tutors/dos/colldet.html
*/
class geo_Rectangle
{

private:

protected:

  //! Upper left coordinate.
  geo::Point<REAL8, 2> d_upperLeft;

  //! Lower right coordinate.
  geo::Point<REAL8, 2> d_lowerRight;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   geo_Rectangle       ();
                   geo_Rectangle       (const geo_Rectangle &rectangle);
                   geo_Rectangle       (REAL8 ulx,
                                        REAL8 uly,
                                        REAL8 lrx,
                                        REAL8 lry);
                   geo_Rectangle       (REAL8 width,
                                        REAL8 height);
                   geo_Rectangle       (const geo::Point<REAL8, 2> &upperLeft,
                                        const geo::Point<REAL8, 2> &lowerRight);

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  void             setUpperLeft        (REAL8 x,
                                        REAL8 y);
  void             setUpperLeft        (const geo::Point<REAL8, 2> &upperLeft);
  void             moveUpperLeft       (REAL8 x,
                                        REAL8 y);
  void             setLowerRight       (REAL8 x,
                                        REAL8 y);
  void             setLowerRight       (const geo::Point<REAL8, 2> &lowerRight);
  void             setWidth            (REAL8 width);
  void             setHeight           (REAL8 height);
  void             setSize             (REAL8 width,
                                        REAL8 height);

  void             setRectangle        (REAL8 x,
                                        REAL8 y,
                                        REAL8 width,
                                        REAL8 height);

  geo_Rectangle &  operator*=          (int i);
  geo_Rectangle &  operator*=          (REAL8 i);

  geo_Rectangle &  operator/=          (int i);
  geo_Rectangle &  operator/=          (REAL8 i);

  geo_Rectangle &  operator+=          (int i);
  geo_Rectangle &  operator+=          (REAL8 i);

  geo_Rectangle &  operator-=          (int i);
  geo_Rectangle &  operator-=          (REAL8 i);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  const geo::Point<REAL8, 2> &getUpperLeft() const;
  const geo::Point<REAL8, 2> &getLowerRight() const;
  REAL8           getLeft             () const;
  REAL8           getTop              () const;
  REAL8           getRight            () const;
  REAL8           getBottom           () const;

  REAL8           left                () const;
  REAL8           top                 () const;
  REAL8           right               () const;
  REAL8           bottom              () const;

  REAL8           getWidth            () const;
  REAL8           getHeight           () const;
  REAL8           getArea             () const;

};



//------------------------------------------------------------------------------
// Free operators.
//------------------------------------------------------------------------------

geo_Rectangle      operator*           (const geo_Rectangle &rectangle,
                                        int                  i);
geo_Rectangle      operator*           (const geo_Rectangle &rectangle,
                                        REAL8               i);

geo_Rectangle      operator/           (const geo_Rectangle &rectangle,
                                        int                  i);
geo_Rectangle      operator/           (const geo_Rectangle &rectangle,
                                        REAL8               i);

geo_Rectangle      operator+           (const geo_Rectangle &rectangle,
                                        int                  i);
geo_Rectangle      operator+           (const geo_Rectangle &rectangle,
                                        REAL8               i);

geo_Rectangle      operator-           (const geo_Rectangle &rectangle,
                                        int                  i);
geo_Rectangle      operator-           (const geo_Rectangle &rectangle,
                                        REAL8               i);



//------------------------------------------------------------------------------
// Free functions.
//------------------------------------------------------------------------------

geo_Rectangle      bounds              (const geo_Rectangle &rect1,
                                        const geo_Rectangle &rect2,
                                        int                  projection);
bool               intersect           (const geo_Rectangle &rect1,
                                        const geo_Rectangle &rect2,
                                        int                  projection);
geo_Rectangle      intersection        (const geo_Rectangle &rect1,
                                        const geo_Rectangle &rect2,
                                        int                  projection);

#endif

