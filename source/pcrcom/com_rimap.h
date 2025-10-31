#ifndef INCLUDED_COM_RIMAP
#define INCLUDED_COM_RIMAP

#include "stddefx.h"
#include "csftypes.h"



namespace com {



/*!
  \class RIMap
  \brief short_description

  longer_description
*/
//       1         2         3         4         5         6         7         8
class RIMap
{

private:

  //! First real.
  REAL8            d_r1{0.0};

  //! Second real.
  REAL8            d_r2{0.0};

  //! First int.
  int              d_i1{0};

  //! Second int.
  int              d_i2{0};

  //! Conversion factor.
  REAL8            d_conv{0.0};

  //! Recalculate the conversion factor.
  void             recalc              ();

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

  //! Constructor.
                   RIMap               ();

  //! Constructor.
                   RIMap               (REAL8 r1,
                                        REAL8 r2,
                                        int i1,
                                        int i2);

  //! Destructor.
  /* virtual */    ~RIMap              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //! Specify the borders of the real interval.
  void             setRealRange        (REAL8 r1,
                                        REAL8 r2);

  //! Specify the borders of the integer interval.
  void             setIntRange         (int i1,
                                        int i2);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  //! Returns the first real border.
  REAL8            r1                  () const;

  //! Returns the second real border.
  REAL8            r2                  () const;

  //! Returns the first integer border.
  int              i1                  () const;

  //! Returns the second integer border.
  int              i2                  () const;

  //! Returns the transformed value of \a v.
  int              transform           (REAL8 v) const;

  //! Returns the transformed value of \a v.
  REAL8            transform           (int v) const;

  //! Returns true if \a r is within the real range.
  bool             inRange             (REAL8 r) const;

  //! Returns true if \a i falls within the int range.
  bool             inRange             (int i) const;

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



} // namespace com

#endif
