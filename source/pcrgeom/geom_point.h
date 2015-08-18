#ifndef INCLUDED_GEOM_POINT
#define INCLUDED_GEOM_POINT



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif



/*!
  \class geom_Point
  \brief The geom_Point class is for simple point objects with integer
         coordinates. Deprecated, see geo::Point
*/
class geom_Point
{

private:

  //! The x-coordinate.
  int              d_x;

  //! The y-coordinate.
  int              d_y;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   geom_Point          ();
                   geom_Point          (int x,
                                        int y);
  /* virtual */    ~geom_Point         ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  void             setX                (int x);
  void             setY                (int y);
  void             setPoint            (int x,
                                        int y);
  geom_Point &     operator+=          (int i);
  void             moveBy              (int dx,
                                        int dy);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  int              getX                () const;
  int              getY                () const;

  //! Returns if \a point is equal to the caller.
  bool             equals              (const geom_Point &point) const;

};


//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------

geom_Point         operator+           (const geom_Point &point,
                                        int i);

bool               operator==          (const geom_Point &lhs,
                                        const geom_Point &rhs);

bool               operator!=          (const geom_Point &lhs,
                                        const geom_Point &rhs);

#endif
