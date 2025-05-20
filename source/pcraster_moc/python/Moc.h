#ifndef INCLUDED_MOC
#define INCLUDED_MOC



// External headers.

// Project headers.
#ifndef INCLUDED_PT_PARTICLETRACKER
#include "pt_ParticleTracker.h"
#define INCLUDED_PT_PARTICLETRACKER
#endif

// Module headers.

#include <pybind11/pybind11.h>
#include <memory>


namespace calc {
  class Field;
}




namespace moc {
namespace python {

//! short_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
/*!
  longer_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED

  \sa        .
*/
class Moc
{

  friend class MocTest;

private:

  pt::ParticleTracker _tracker;

protected:

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   Moc                 (geo::RasterSpace const& space,
                                        double timeIncrement,
                                        UINT4 nrParticles,
                                        calc::Field const* initialConcentration,
                                        calc::Field const* effectivePorosity,
                                        calc::Field const* storageCoefficient);

                   Moc                 (const Moc&) = delete;

  Moc&             operator=           (const Moc&) = delete;

  /* virtual */    ~Moc                ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  pybind11::tuple  transport           (
                        calc::Field const* flux,
                        calc::Field const* xVelocity,
                        calc::Field const* yVelocity,
                        calc::Field const* longitudinalDispersionCoefficient,
                        calc::Field const* transverseDispersionCoefficient,
                        calc::Field const* hydraulicHead,
                        calc::Field const* saturatedThickness);

  std::shared_ptr<calc::Field> adjust(
                                  calc::Field const* changeInConcentration);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

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


} // namespace python
} // namespace moc

#endif
