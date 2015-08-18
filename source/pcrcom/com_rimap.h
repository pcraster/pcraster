#ifndef INCLUDED_COM_RIMAP
#define INCLUDED_COM_RIMAP



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CSFTYPES
#include "csftypes.h"
#define INCLUDED_CSFTYPES
#endif



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
  REAL8            d_r1;

  //! Second real.
  REAL8            d_r2;

  //! First int.
  int              d_i1;

  //! Second int.
  int              d_i2;

  //! Conversion factor.
  REAL8            d_conv;

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
