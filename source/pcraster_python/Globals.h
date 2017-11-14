#ifndef INCLUDED_PCRASTER_PYTHON_GLOBALS
#define INCLUDED_PCRASTER_PYTHON_GLOBALS

#include "pcraster_python_export.h"

// External headers.

// Project headers.
#ifndef INCLUDED_DAL_CLIENT
#include "dal_Client.h"
#define INCLUDED_DAL_CLIENT
#endif

#ifndef INCLUDED_GEO_RASTERSPACE
#include "geo_rasterspace.h"
#define INCLUDED_GEO_RASTERSPACE
#endif

// Module headers.



namespace dal {
  class RasterDal;
}
namespace calc {
  class RunTimeEngine;
}



namespace pcraster {
namespace python {

//! short_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
/*!
  longer_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED

  \sa        .
*/
class Globals: public dal::Client
{

  friend class GlobalsTest;

private:

  //! Clone.
  geo::RasterSpace _cloneSpace;

  //! Runtime enginge.
  calc::RunTimeEngine* _rte;

  dal::RasterDal* _rasterDal;

protected:

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   Globals             ();

                   ~Globals            ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  void             init                ();

  void             setCloneSpace       (geo::RasterSpace const& space);

  void             setRandomSeed       (unsigned int seed);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  PCRASTER_PYTHON_EXPORT geo::RasterSpace const& cloneSpace   ();

  calc::RunTimeEngine& rte             ();

  dal::RasterDal&  rasterDal           ();

};

PCRASTER_PYTHON_EXPORT extern Globals globals;

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
} // namespace pcraster

#endif
