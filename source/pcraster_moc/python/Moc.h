#ifndef INCLUDED_MOC
#define INCLUDED_MOC



// External headers.
#ifndef INCLUDED_BOOST_NONCOPYABLE
#include <boost/noncopyable.hpp>
#define INCLUDED_BOOST_NONCOPYABLE
#endif

#ifndef INCLUDED_BOOST_PYTHON_TUPLE
#include <boost/python/tuple.hpp>
#define INCLUDED_BOOST_PYTHON_TUPLE
#endif

#ifndef INCLUDED_BOOST_SHARED_PTR
#include <boost/shared_ptr.hpp>
#define INCLUDED_BOOST_SHARED_PTR
#endif

// Project headers.
#ifndef INCLUDED_PT_PARTICLETRACKER
#include "pt_ParticleTracker.h"
#define INCLUDED_PT_PARTICLETRACKER
#endif

// Module headers.



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
class Moc: private boost::noncopyable
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

  /* virtual */    ~Moc                ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  boost::python::tuple transport       (
                        calc::Field const* flux,
                        calc::Field const* xVelocity,
                        calc::Field const* yVelocity,
                        calc::Field const* longitudinalDispersionCoefficient,
                        calc::Field const* transverseDispersionCoefficient,
                        calc::Field const* hydraulicHead,
                        calc::Field const* saturatedThickness);

  boost::shared_ptr<calc::Field> adjust(
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
