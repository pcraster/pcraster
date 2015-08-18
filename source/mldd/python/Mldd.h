#ifndef INCLUDED_MLDD
#define INCLUDED_MLDD



// External headers.
#ifndef INCLUDED_BOOST_PYTHON_TUPLE
#include <boost/python/tuple.hpp>
#define INCLUDED_BOOST_PYTHON_TUPLE
#endif

#ifndef INCLUDED_BOOST_NONCOPYABLE
#include <boost/noncopyable.hpp>
#define INCLUDED_BOOST_NONCOPYABLE
#endif

#ifndef INCLUDED_BOOST_SHARED_PTR
#include <boost/shared_ptr.hpp>
#define INCLUDED_BOOST_SHARED_PTR
#endif

// Project headers.

// Module headers.
#ifndef INCLUDED_MLDD_MLDD
#include "mldd_mldd.h"
#define INCLUDED_MLDD_MLDD
#endif



namespace calc {
  class Field;
}



namespace mldd {
namespace python {

//! Wrapper class to interface between mldd::Mldd and the Python extension.
/*!
  This class does the necessary things to allow mldd::Mldd to be used from
  Python.
*/
class Mldd: private boost::noncopyable
{

  friend class MlddTest;

private:

  mldd::Mldd       _mldd;

protected:

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   Mldd                (geo::RasterSpace const& space);

  /* virtual */    ~Mldd               ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  void             setDem              (calc::Field const* dem);

  void             setStream           (calc::Field const* ldd1,
                                        calc::Field const* ldd2,
                                        calc::Field const* ldd3,
                                        calc::Field const* ldd4,
                                        calc::Field const* ldd5,
                                        calc::Field const* ldd6,
                                        calc::Field const* ldd7,
                                        calc::Field const* ldd8);

  void             addStream           (calc::Field const* ldd);

  void             removeStream        (calc::Field const* mark1,
                                        calc::Field const* mark2,
                                        calc::Field const* mark3,
                                        calc::Field const* mark4,
                                        calc::Field const* mark5,
                                        calc::Field const* mark6,
                                        calc::Field const* mark7,
                                        calc::Field const* mark8);

  boost::shared_ptr<calc::Field> diffuse(
                                        calc::Field const* oldState,
                                        calc::Field const* area,
                                        calc::Field const* fixedHead,
                                        calc::Field const* value1,
                                        calc::Field const* value2,
                                        calc::Field const* value3,
                                        calc::Field const* value4,
                                        calc::Field const* value5,
                                        calc::Field const* value6,
                                        calc::Field const* value7,
                                        calc::Field const* value8,
                                        INT4 nrIterations);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  boost::python::tuple getStream       () const;

  boost::python::tuple getWeight       () const;

  boost::shared_ptr<calc::Field> getDem() const;

  boost::shared_ptr<calc::Field> upstream(
                                        calc::Field const* material);

  boost::shared_ptr<calc::Field> accuflux(
                                        calc::Field const* material);

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
} // namespace mldd

#endif
