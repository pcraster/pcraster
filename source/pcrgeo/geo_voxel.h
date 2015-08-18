#ifndef INCLUDED_GEO_VOXEL
#define INCLUDED_GEO_VOXEL



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CSF
#include "csf.h"
#define INCLUDED_CSF
#endif



namespace geo {

const int Sand = 1;
const int Shale = 2;

/*!
  \class Voxel
  \brief short_description

  longer_description
*/
//       1         2         3         4         5         6         7         8
class Voxel
{

private:

  //! Sediment type.
  INT4             d_sedType;

  //! Current thickness.
  REAL8            d_thickness;

  //! Original thickness.
  REAL8            d_origThickness;

  //! Frees dynamically allocated memory.
  void             clean               ();

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

  //! Constructor.
                   Voxel               (INT4 s);

  //! Constructor.
                   Voxel               (INT4 s,
                                        REAL8 t);
  //! Constructor.
                   Voxel               (INT4 s,
                                        REAL8 t,
                                        REAL8 ot);

  //! Copy constructor.
                   Voxel               (const Voxel &rhs);

  //! Destructor.
  /* virtual */    ~Voxel              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //! Assignment operator.
  Voxel &          operator=           (const Voxel &rhs);

  //! Sets the thickness to \a t.
  void             setThickness        (REAL8 t);

  //! Sets the original thickness to \a t.
  void             setOrigThickness    (REAL8 t);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  //! Returns the sediment type.
  INT4             sedType             () const;

  //! Returns the thickness.
  REAL8            thickness           () const;

  //! Returns the original thickness.
  REAL8            origThickness       () const;

};



//------------------------------------------------------------------------------
// INLINE FUNCIONS
//------------------------------------------------------------------------------

inline void geo::Voxel::setThickness(REAL8 t)
{ d_thickness = t; }

inline void geo::Voxel::setOrigThickness(REAL8 t)
{ d_origThickness = t; }

inline INT4 geo::Voxel::sedType() const
{ return d_sedType; }

inline REAL8 geo::Voxel::thickness() const
{ return d_thickness; }

inline REAL8 geo::Voxel::origThickness() const
{ return d_origThickness; }



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace geo

#endif
