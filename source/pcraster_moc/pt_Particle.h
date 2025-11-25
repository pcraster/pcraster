#ifndef INCLUDED_PT_PARTICLE
#define INCLUDED_PT_PARTICLE

#include "stddefx.h"
#include "geo_point.h"



namespace geo {
  class CellLoc;
}
namespace pt {
  // Particle declarations.
}



namespace pt {



//! This class is for particle object used in particle tracking.
/*!
  Particles have a concentration which can be set and queried.
*/
class Particle: public geo::Point<double, 2>
{

private:

  //! Row of cell where particle originated.
  size_t           d_birthRow;

  //! Col of cell where particle originated.
  size_t           d_birthCol;

  //! Concentration of particle.
  double d_concentration;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   Particle            (size_t birthRow = 0,
                                        size_t birthCol = 0,
                                        double x = 0.0,
                                        double y = 0.0,
                                        double concentration = 0.0);

  /* virtual */    ~Particle           ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  void             setBirthCell        (size_t row,
                                        size_t col);

  void             setConcentration    (double concentration);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  size_t           birthRow            () const;

  size_t           birthCol            () const;

  bool             born                (size_t row,
                                        size_t col) const;

  bool             born                (const geo::CellLoc& loc) const;

  double           concentration       () const;

};



//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace pt

#endif
