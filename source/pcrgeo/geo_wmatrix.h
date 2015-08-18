#ifndef INCLUDED_GEO_WMATRIX
#define INCLUDED_GEO_WMATRIX



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CSF
#include "csf.h"
#define INCLUDED_CSF
#endif

#ifndef INCLUDED_GEO_POINT
#include "geo_point.h"
#define INCLUDED_GEO_POINT
#endif



namespace geo {



/*!
  \class WMatrix
  \brief short_description

  longer_description
*/
//       1         2         3         4         5         6         7         8
class WMatrix
{

private:

  REAL8            d_m11, d_m12, d_m13;
  REAL8            d_m21, d_m22, d_m23;
  REAL8            d_m31, d_m32, d_m33;
  REAL8            d_dx,  d_dy,  d_dz;

  void             bmul                (const WMatrix &m);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

  //! Constructor.
                   WMatrix             ();

  //! Copy constructor.
                   WMatrix             (const WMatrix &m);

  //! Constructor.
                   WMatrix             (REAL8 m11,
                                        REAL8 m12,
                                        REAL8 m13,
                                        REAL8 m21,
                                        REAL8 m22,
                                        REAL8 m23,
                                        REAL8 m31,
                                        REAL8 m32,
                                        REAL8 m33,
                                        REAL8 dx,
                                        REAL8 dy,
                                        REAL8 dz);

  //! Destructor.
  /* virtual */    ~WMatrix            ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //! Assignment operator.
  WMatrix &        operator=           (const WMatrix &rhs);

  void             reset               ();

  void             setMatrix           (REAL8 m11,
                                        REAL8 m12,
                                        REAL8 m13,
                                        REAL8 m21,
                                        REAL8 m22,
                                        REAL8 m23,
                                        REAL8 m31,
                                        REAL8 m32,
                                        REAL8 m33,
                                        REAL8 dx,
                                        REAL8 dy,
                                        REAL8 dz);

  void             translate           (REAL8 dx,
                                        REAL8 dy,
                                        REAL8 dz);

  void             translate           (const Point<REAL8, 3> &p);

  void             scale               (REAL8 sx,
                                        REAL8 sy,
                                        REAL8 sz);

  void             scale               (const Point<REAL8, 3> &p);

  void             rotate              (REAL8 rx,
                                        REAL8 ry,
                                        REAL8 rz);

  void             rotate              (const Point<REAL8, 3> &p);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  REAL8            m11                 () const { return d_m11; }

  REAL8            m12                 () const { return d_m12; }

  REAL8            m13                 () const { return d_m13; }

  REAL8            m21                 () const { return d_m21; }

  REAL8            m22                 () const { return d_m22; }

  REAL8            m23                 () const { return d_m23; }

  REAL8            m31                 () const { return d_m31; }

  REAL8            m32                 () const { return d_m32; }

  REAL8            m33                 () const { return d_m33; }

  REAL8            dx                  () const { return d_dx; }

  REAL8            dy                  () const { return d_dy; }

  REAL8            dz                  () const { return d_dz; }

  void             map                 (REAL8 x,
                                        REAL8 y,
                                        REAL8 z,
                                        REAL8 *tx,
                                        REAL8 *ty,
                                        REAL8 *tz);

};



//------------------------------------------------------------------------------
// INLINE FUNCIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace geo

#endif
